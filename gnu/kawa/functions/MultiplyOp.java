package gnu.kawa.functions;
import gnu.math.IntNum;
import gnu.math.Numeric;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "*".
 * @author Per Bothner
 */

public class MultiplyOp extends ProcedureN
{
  public static final MultiplyOp $St = new MultiplyOp("*");

  public MultiplyOp(String name)
  {
    super(name);
  }

  public static Object apply (Object arg1, Object arg2)
  {
    return ((Numeric) arg1).mul(arg2);
  }

  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.one ();
    Numeric result = (Numeric) args[0];
    for (int i = 1; i < len; i++)
      result = result.mul (args[i]);
    return result;
   }
}
