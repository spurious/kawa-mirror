package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;
import gnu.math.Numeric;

/**
 * Implement the Scheme standard function "gcd".
 * @author Per Bothner
 */

public class gcd extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    IntNum result = (IntNum) args[0];
    for (int i = 1; i < len; i++)
      result = IntNum.gcd (result, (IntNum) args[i]);
    return result;
   }
}
