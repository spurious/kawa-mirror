// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.kawa.functions.NumberCompare;
import gnu.math.*;
import gnu.expr.*;

/** Compares two values (or sequences) according to XPath semantics. */

public class Compare extends Procedure2 implements CanInline
{
  static final int RESULT_GRT = 1;
  static final int RESULT_EQU = 0;
  static final int RESULT_LSS = -1;
  static final int RESULT_NAN = -2;
  static final int RESULT_NEQ = -3;

  // One flag bit for each of the above RESULT_XXX codes:
  static final int TRUE_IF_GRT = 1 << (RESULT_GRT + 3);
  static final int TRUE_IF_EQU = 1 << (RESULT_EQU + 3);
  static final int TRUE_IF_LSS = 1 << (RESULT_LSS + 3);
  static final int TRUE_IF_NAN = 1 << (RESULT_NAN + 3);
  static final int TRUE_IF_NEQ = 1 << (RESULT_NEQ + 3);

  int flags;

  public static Compare make(String name, int flags)
  {
    Compare proc = new Compare();
    proc.setName(name);
    proc.flags = flags;
    return proc;
  }

  /*
  private static Object toString(object value)
  {
    if (value instanceof TreeList)
      return = ((TreeList) value).stringValue(0, sbuf);
    else if (value instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) value;
	if (pos.sequence instanceof TreeList)
	  {
	    ((TreeList) pos.sequence).stringValue(pos.ipos >> 1, sbuf);
	    return;
	  }
      }
  }
  */

  public static boolean apply(int flags, Object arg1, Object arg2)
  {
    if (arg1 instanceof Values)
      {
	Values values1 = (Values) arg1;
	int index = 0;
	for (;;)
	  {
	    int next = values1.nextDataIndex(index);
	    if (next < 0)
	      return false;
	    if (apply(flags, values1.getNext(index << 1, null), arg2))
	      return true;
	    index = next;
	  }
      }
    if (arg2 instanceof Values)
      {
	Values values2 = (Values) arg2;
	int index = 0;
	for (;;)
	  {
	    int next = values2.nextDataIndex(index);
	    if (next < 0)
	      return false;
	    if (apply(flags, arg1, values2.getNext(index << 1, null)))
	      return true;
	    index = next;
	  }
      }
    if (arg1 instanceof Number || arg2 instanceof Number)
      {
	if (! (arg1 instanceof Numeric))
	  arg1 = new DFloNum(StringValue.stringValue(arg1));
	if (! (arg2 instanceof Numeric))
	  arg2 = new DFloNum(StringValue.stringValue(arg2));
	return NumberCompare.apply2(flags, arg1, arg2);
      }
    String str1 = StringValue.stringValue(arg1);
    String str2 = StringValue.stringValue(arg2);
    int comp = str1.compareTo(str2);
    if (comp < 0)
      return (flags & TRUE_IF_LSS+TRUE_IF_NEQ) != 0;
    else if (comp > 0)
      return (flags & TRUE_IF_GRT+TRUE_IF_NEQ) != 0;
    else
      return (flags & TRUE_IF_EQU) != 0;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return apply(flags, arg1, arg2) ? Boolean.TRUE : Boolean.FALSE;
  }

  public static final Compare $Eq   = make("=",TRUE_IF_EQU);
  public static final Compare $Ex$Eq
  = make("!=",TRUE_IF_GRT|TRUE_IF_LSS|TRUE_IF_NEQ);
  public static final Compare $Gr   = make(">",TRUE_IF_GRT);
  public static final Compare $Gr$Eq= make(">=",TRUE_IF_GRT|TRUE_IF_EQU);
  public static final Compare $Ls   = make("<",TRUE_IF_LSS);
  public static final Compare $Ls$Eq= make("<=",TRUE_IF_LSS|TRUE_IF_EQU);

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression folded = ApplyExp.inlineIfConstant(this, exp);
    if (folded != exp)
      return folded;
    return exp;
  }
}
