// Copyright (c) 1998  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** Maintains the state for generating a switch statement. */

public class SwitchState
{
  /** The smallest case value, so far. */
  int minValue;
  /** The largest case value, so far. */
  int maxValue;
  /** The number of cases (not including the default case). */
  int numCases;
  /** The case values, in numerical order (in values[0..numCases-1]). */
  int[] values;
  /** The case locations, in the same order as values. */
  Label[] labels;
  /** The location to jump to if none of the cases match. */
  Label defaultLabel;
  /* Location of the actual switch instruction. */
  Label switch_label;
  Type[] typeState;

  public int getMaxValue() { return maxValue; }

  public SwitchState(CodeAttr code)
  {
    switch_label = new Label(code);

    code.popType();  // pop switch value

    // Save stack types (except top int) into typeState
    if (code.SP > 0)
      {
	typeState = new Type[code.SP];
	System.arraycopy(code.stack_types, 0, typeState, 0, code.SP);
      }

    code.emitGoto(switch_label);

    numCases = 0;
  }

  void restoreStack(CodeAttr code)
  {
    if (typeState == null)
      code.SP = 0;
    else
      {
	code.SP = typeState.length;
	System.arraycopy(typeState, 0, code.stack_types, 0, code.SP);
      }
  }

  /** Emit a new case, for the given value, whose label is here. */
  public boolean addCase(int value, CodeAttr code)
  {
    Label label = new Label(code);
    boolean ok = addCase (value, label, code);
    label.define(code);
    restoreStack(code);
    return ok;
  }

  public void addDefault(CodeAttr code)
  {
    Label label = new Label(code);
    label.define(code);
    defaultLabel = label;
    restoreStack(code);
  }

  /** Add a new case.
   * @param value the case value to match against at run-time
   * @param label the location to go to if the value matches
   * @param code the CodeAttr of the Method we are generating code for
   * @return true on success;  false if value duplicates an existing value
   */
  public boolean addCase(int value, Label label, CodeAttr code)
  {
    if (values == null)
      {
	values = new int[10];
	labels = new Label[10];
	numCases = 1;
	minValue = maxValue = value;
	values[0] = value;
	labels[0] = label;
	return true;
      }
    int[] old_values = values;
    Label[] old_labels = labels;
    if (numCases >= values.length)
      {
	values = new int[2 * numCases];
	labels = new Label[2 * numCases];
      }
    int copyBefore;
    if (value < minValue)
      {
	copyBefore = 0;
	minValue = value;
      }
    else if (value > maxValue)
      {
	copyBefore = numCases;
	maxValue = value;
      }
    else
      {
	// Binary search.
	copyBefore = 0;
	int num = numCases;
	while (num > 1)
	  {
	    int half = num >> 1;
	    int mid = copyBefore + half;
	    if (value >= values[mid])
	      copyBefore = mid;
	    num = half;
	  }
	if (value == values[copyBefore])
	  return false;
      }
    int copyAfter = numCases = copyBefore;
    System.arraycopy(old_values, copyBefore, values, copyBefore+1, copyAfter);
    System.arraycopy(old_values, 0, values, 0, copyBefore);
    values[copyBefore] = value;
    System.arraycopy(old_labels, copyBefore, labels, copyBefore+1, copyAfter);
    System.arraycopy(old_labels, 0, labels, 0, copyBefore);
    labels[copyBefore] = label;
    return true;
  }

  /** Handle the end of the switch statement.
   * Assume the case value is on the stack; go to the matching case label. */
  public void finish (CodeAttr code)
  {
    switch_label.define(code);
    if (numCases <= 1)
      {
	if (numCases == 1)
	  {
	    code.emitPushInt(minValue);
	    code.emitGotoIfEq(labels[0]);
	  }
	else
	  {
	    code.emitPop(1);
	  }
	code.emitGoto(defaultLabel);
	return;
      }
    int start = code.PC;
    int pad = (3 - start) & 3;
    if (2 * numCases >= maxValue - minValue)
      {
	code.reserve(13 + pad + 4 * (maxValue - minValue + 1));
	code.put1(170);  // tableswitch
	while (--pad >= 0) code.put1(0);
	defaultLabel.emit_wide(code, start);
	code.put4(minValue);
	code.put4(maxValue);
	int index = 0;
	for (int i = minValue;  i <= maxValue;  i++)
	  {
	    Label lab = values[index] == i ? labels[index++] : defaultLabel;
	    defaultLabel.emit_wide(code, start);
	  }
      }
    else
      {
	code.reserve(9 + pad + 8 * numCases);
	code.put1(171);  // lookupswitch
	while (--pad >= 0) code.put1(0);
	defaultLabel.emit_wide(code, start);
	code.put4(numCases);
	for (int index = 0;  index < numCases;  index++)
	  {
	    code.put4(values[index]);
	    labels[index].emit_wide(code, start);
	  }
      }
  }
}
