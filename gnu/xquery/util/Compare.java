// Copyright (c) 2001, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.kawa.functions.NumberCompare;
import gnu.math.*;
import gnu.expr.*;
import gnu.kawa.xml.KNode;
import gnu.kawa.xml.UntypedAtomic;

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

  public static boolean apply(int flags, Object arg1, Object arg2,
                              NamedCollator collator)
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
	    if (apply(flags, values1.getPosNext(index << 1), arg2, collator))
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
	    if (apply(flags, arg1, values2.getPosNext(index << 1), collator))
	      return true;
	    index = next;
	  }
      }
    arg1 = KNode.atomicValue(arg1);
    arg2 = KNode.atomicValue(arg2);
    if (arg1 instanceof Number || arg2 instanceof Number)
      {
	if (arg1 instanceof UntypedAtomic)
          {
            String str = arg1.toString();
            if (arg2 instanceof DateTime)
              arg1 = DateTime.parse(str, ((DateTime) arg2).components());
            else if (arg2 instanceof Duration)
              arg1 = Duration.parse(str, ((Duration) arg2).unit());
            else
              arg1 = new DFloNum(str);
          }
	if (arg2 instanceof UntypedAtomic)
          {
            String str = arg2.toString();
            if (arg1 instanceof DateTime)
              arg2 = DateTime.parse(str, ((DateTime) arg1).components());
            else if (arg1 instanceof Duration)
              arg2 = Duration.parse(str, ((Duration) arg1).unit());
            else
              arg2 = new DFloNum(str);
          }
	return NumberCompare.apply2(flags, arg1, arg2);
      }
    if (arg1 instanceof UntypedAtomic)
      {
        if (arg2 instanceof String || arg2 instanceof UntypedAtomic)
          arg1 = arg1.toString();
        /*
        else
          arg1 = XDataType.getTypeOf(arg2).cast(arg1);
        */
      }
    if (arg2 instanceof UntypedAtomic)
      {
        if (arg1 instanceof String || arg1 instanceof UntypedAtomic)
          arg2 = arg2.toString();
        /*
        else
          arg2 = XDataType.getTypeOf(arg1).cast(arg2);
        */
      }
    int comp;
    if (arg1 instanceof Symbol && arg2 instanceof Symbol)
      comp = arg1.equals(arg2) ? 0 : -3;
    else
      {
        String str1 = arg1.toString();
        String str2 = arg2.toString();
        /* #ifdef JAVA2 */
        if (collator != null)
          comp = collator.compare(str1, str2);
        else
        /* #endif */
          comp = str1.compareTo(str2);
      }
    if (comp < 0)
      return (flags & TRUE_IF_LSS+TRUE_IF_NEQ) != 0;
    else if (comp > 0)
      return (flags & TRUE_IF_GRT+TRUE_IF_NEQ) != 0;
    else
      return (flags & TRUE_IF_EQU) != 0;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return apply(flags, arg1, arg2, null) ? Boolean.TRUE : Boolean.FALSE;
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
    Expression folded = exp.inlineIfConstant(this, walker);
    if (folded != exp)
      return folded;
    return exp;
  }
}
