package kawa.standard;
import kawa.lang.*;
import gnu.math.RealNum;
import gnu.math.Numeric;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "max".
 * @author Per Bothner
 */

public class max extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    RealNum result = (RealNum) args[0];
    for (int i = 1; i < len; i++)
      result = result.max ((RealNum) args[i]);
    return result;
   }
}
