package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;
import kawa.math.Numeric;

/**
 * Implement the Scheme standard function "+".
 * @author Per Bothner
 */

public class plus_oper extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    Numeric result = (Numeric) args[0];
    for (int i = 1; i < len; i++)
      result = result.add (args[i]);
    return result;
   }
}
