package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;
import gnu.math.Numeric;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "+".
 * @author Per Bothner
 */

public class plus_oper extends ProcedureN
{
  public static Object apply$V(Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    Numeric result = (Numeric) args[0];
    for (int i = 1; i < len; i++)
      result = result.add (args[i]);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return apply$V(args);
  }
}
