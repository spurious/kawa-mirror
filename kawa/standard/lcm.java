package kawa.standard;
import gnu.math.IntNum;
import gnu.math.Numeric;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "lcm".
 * @author Per Bothner
 */

public class lcm extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.one ();
    IntNum result = IntNum.abs ((IntNum) args[0]);
    for (int i = 1; i < len; i++)
      result = IntNum.lcm (result, (IntNum) args[i]);
    return result;
   }
}
