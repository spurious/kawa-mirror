package kawa.standard;
import kawa.lang.*;
import kawa.math.RealNum;
import kawa.math.Numeric;

/**
 * Implement the Scheme standard function "max".
 * @author Per Bothner
 */

public class max extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int len = args.length;
    RealNum result = (RealNum) args[0];
    for (int i = 1; i < len; i++)
      result = result.max ((RealNum) args[i]);
    return result;
   }
}
