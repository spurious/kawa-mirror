package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "-".
 * @author Per Bothner
 */

public class minus_oper extends ProcedureN
{
  public static Object apply (Object arg1)
  {
    return ((Numeric) arg1).neg();
  }

  public static Object apply (Object arg1, Object arg2)
  {
    return ((Numeric) arg1).sub(arg2);
  }

  public Object applyN (Object[] args)
  {
    Numeric result = (Numeric) args[0];
    if (args.length == 1)
      return result.neg ();
    for (int i = 1; i < args.length;  i++)
      result = result.sub (args[i]);
    return result;
   }
}
