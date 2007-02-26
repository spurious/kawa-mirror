package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.UnescapedData;

public class MakeUnescapedData extends Procedure1 implements CanInline
{
  public static final MakeUnescapedData unescapedData
    = new MakeUnescapedData();

  public Object apply1(Object arg)
  {
    return new UnescapedData(arg == null ? "" : arg.toString());
  }

  public Expression inline (ApplyExp exp, InlineCalls walker)
  {
    exp.walkArgs(walker);
    Expression[] args = exp.getArgs();
    if (args.length == 1 && args[0] instanceof QuoteExp)
      return new QuoteExp(apply1(((QuoteExp) args[0]).getValue()));
    return exp;
  }
}
