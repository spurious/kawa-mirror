package gnu.xquery.util;
import gnu.kawa.functions.Convert;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.kawa.reflect.Invoke;

public class CastAs extends Convert
{
  public static final CastAs castAs = new CastAs();

  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = (Type) arg1;
    if (type instanceof XDataType)
      return ((XDataType) type).cast(arg2);
    return super.apply2(arg1, arg2);
  }

  static final ClassType typeXDataType =
    ClassType.make("gnu.kawa.xml.XDataType");
  static final Method castMethod = typeXDataType.getDeclaredMethod("cast", 1);

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    exp = Invoke.inlineClassName(exp, 0, (InlineCalls) walker);
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[0] instanceof QuoteExp))
      return exp;
    Object type = ((QuoteExp) args[0]).getValue();
    if (type instanceof XDataType)
      {
        XDataType xtype = (XDataType) type;
        return new ApplyExp(castMethod, args);
      }
    return exp;
  }

}
