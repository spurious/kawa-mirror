package kawa.standard;
import kawa.lang.*;
import gnu.math.RealNum;
import gnu.math.Numeric;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "min".
 * @author Per Bothner
 */

public class min extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    RealNum result = (RealNum) args[0];
    for (int i = 1; i < len; i++)
      result = result.min ((RealNum) args[i]);
    return result;
   }
}
