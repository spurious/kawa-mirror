// Copyright (c) 1997, 2004, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/**
 * A Label represents a location in a Code attribute.
 */

public class Label {

  /** Offset of definition in the fixup_offsets and fixup_labels arrays.
   * The offset corresponds to the fixup itself. */
  int first_fixup;

  /** The PC of where the label is, or -1 if not yet defined.
   * The value -2 means don't generate a StackMapTable entry.
   * This PC may be tentative if we later run processFixups.
   * The offset in the code array is cattr.fixupOffset(first_fixup). */
  int position;

  // FIXME Probably more efficient to have a single array:
  // local-types followed by stack-types.  We'd need an extra short field.
  Type[] stackTypes;
  Type[] localTypes;

  public final boolean defined () { return position >= 0; }

  public Label ()
  {
    this(-1);
  }

  public Label (CodeAttr code)
  {
    this(-1);
  }

  public Label (int position)
  {
    this.position = position;
  }

  void setTypes (Type[] locals, int usedLocals,
                 Type[] stack, int usedStack)
  {
    for (; usedLocals > 0; usedLocals--)
      {
        Type last = locals[usedLocals-1];
        if (last != null)
          break;
      }
    if (stackTypes == null)
      {
        if (usedStack == 0)
          stackTypes = Type.typeArray0;
        else
          {
            stackTypes = new Type[usedStack];
            System.arraycopy(stack, 0, stackTypes, 0, usedStack);
          }
        if (usedLocals == 0)
            localTypes = Type.typeArray0;
        else
          {
            localTypes = new Type[usedLocals];
            System.arraycopy(locals, 0, localTypes, 0, usedLocals);
          }
      }
    else
      {
        int SP = usedStack;
        int slen = stackTypes.length;
        if (SP != slen)
          //          throw new Error();
          throw new Error("inconsistent stack len was:"+slen+" now:"+SP+" for "+this);
        for (int i = 0; i < SP; i++)
          {
            stackTypes[i] = mergeTypes(stackTypes[i], stack[i]);
          }
        int min = usedLocals < localTypes.length ? usedLocals : localTypes.length ;
        for (int i = 0; i < min; i++)
          {
            localTypes[i] = mergeTypes(localTypes[i], locals[i]);
          }
        for (int i = usedLocals; i < localTypes.length;  i++)
          localTypes[i] = null;
      }
  }

  Type mergeTypes (Type t1, Type t2)
  {
    if ((t1 instanceof PrimType) != (t2 instanceof PrimType))
      return null;
    return Type.lowestCommonSuperType(t1, t2);
  }

  public void setTypes (CodeAttr code)
  {
    if (stackTypes != null && code.SP != stackTypes.length)
      throw new Error();
    setTypes(code.local_types,
             code.local_types == null ? 0 : code.local_types.length,
             code.stack_types,
             code.SP);
  }

  public void setTypes (Label other)
  {
    setTypes(other.localTypes, other.localTypes.length,
             other.stackTypes, other.stackTypes.length);
  }

  void setTypesSame (Label other)
  {
    stackTypes = other.stackTypes;
    localTypes = other.localTypes;
  }

  /**
   * Define the value of a label as having the current location.
   * @param code the "Code" attribute of the current method
   */
  public void defineRaw (CodeAttr code)
  {
    if (position >= 0)
      throw new Error ("label definition more than once");
    position = code.PC;
    first_fixup = code.fixup_count;
    if (first_fixup >= 0)
      code.fixupAdd(CodeAttr.FIXUP_DEFINE, this);
  }

  /**
   * Define the value of a label as having the current location.
   * @param code the "Code" attribute of the current method
   */
  public void define (CodeAttr code)
  {
    if (code.reachableHere())
      setTypes(code);
    if (localTypes != null)
      // Copy merged type back to current state.
      code.setTypes(this);
    code.setReachable(true);
    defineRaw(code);
  }

  /* DEBUG
  int id = ++counter;
  static int counter;
  public String toString() { return "Label#"+id+"-pos:"+position; }
  */
}
