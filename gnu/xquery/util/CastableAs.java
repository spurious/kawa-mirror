package gnu.xquery.util;
import gnu.kawa.reflect.*;
import gnu.xquery.lang.XQuery;
import gnu.bytecode.*;
import gnu.kawa.xml.*;
import gnu.expr.*;

public class CastableAs extends InstanceOf
{
  public static CastableAs castableAs = new CastableAs();

  CastableAs ()
  {
    super(XQuery.getInstance(), "castable as");
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = language.asType(arg2);
    boolean result;
    if (type instanceof XDataType)
      result = ((XDataType) type).castable(arg1);
    else
      result = type.isInstance(arg1);
    return language.booleanObject(result);
  }

  static final Method castableMethod
    = CastAs.typeXDataType.getDeclaredMethod("castable", 1);

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    exp = Invoke.inlineClassName(exp, 1, walker);
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[1] instanceof QuoteExp))
      return exp;
    Object type = ((QuoteExp) args[1]).getValue();
    if (type instanceof XDataType)
      return new ApplyExp(castableMethod,
                          new Expression[] { args[1], args[0] });
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    // To override InstanceOf.compile.
    ApplyExp.compile(exp, comp, target);
  }
}
