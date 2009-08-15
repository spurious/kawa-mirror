package gnu.xquery.util;
import gnu.expr.*;
import gnu.mapping.Procedure;
import gnu.bytecode.ClassType;
import gnu.bytecode.Type;
import gnu.kawa.functions.NumberCompare;
import gnu.kawa.xml.*;
import gnu.math.*;
import gnu.xquery.lang.XQuery;

public class CompileMisc
{
  /** Inliner for the Compare procedure. */
  public static Expression inlineCompare
  (ApplyExp exp, InlineCalls walker,
   boolean argsInlined, Procedure proc)
  {
    exp.walkArgs(walker, argsInlined);
    Expression folded = exp.inlineIfConstant(proc, walker);
    if (folded != exp)
      return folded;
    Compare cproc = (Compare) proc;
    if ((cproc.flags & Compare.VALUE_COMPARISON) != 0)
      {
      }
    else
      {
        exp = new ApplyExp(ClassType.make("gnu.xquery.util.Compare")
                           .getDeclaredMethod("apply", 4),
                           new Expression[] { new QuoteExp(IntNum.make(cproc.flags)),
                                              exp.getArg(0),
                                              exp.getArg(1),
                                              QuoteExp.nullExp });
      }
    if (exp.getTypeRaw() == null)
      exp.setType(XDataType.booleanType);
    return exp;
  }

  /** Inliner for the BooleanValue procedure. */
  public static Expression inlineBooleanValue
  (ApplyExp exp, InlineCalls walker,
   boolean argsInlined, Procedure proc)
  {
    exp.walkArgs(walker, argsInlined);
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      {
        Expression arg = args[0];
        Type type = arg.getType();
        if (type == XDataType.booleanType)
          return arg;
        if (type == null)
          exp.setType(XDataType.booleanType);
        if (arg instanceof QuoteExp)
          {
            Object value = ((QuoteExp) arg).getValue();
            try
              {
                return BooleanValue.booleanValue(value) ? XQuery.trueExp : XQuery.falseExp;
              }
            catch (Throwable ex)
              {
                String message = "cannot convert to a boolean";
                walker.getMessages().error('e', message);
                return new ErrorExp(message);
              }
          }
      }
    return exp;
  }

  /** Inliner for the ArithOp procedure. */
  public static Expression inlineArithOp
  (ApplyExp exp, InlineCalls walker,
   boolean argsInlined, Procedure proc)
  {
    exp.walkArgs(walker, argsInlined);
    // FUTURE
    return exp;
  }
}
