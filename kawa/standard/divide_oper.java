package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/**
 * Implement the Scheme standard function "/".
 * @author Per Bothner
 */

public class divide_oper extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Numeric result;
    int i = 0;
    if (args.length == 1)
      result = IntNum.one ();
    else
      result = (Numeric) (args[i++]);
    for (; i < args.length;  i++)
      result = result.div (args[i]);
    return result;
   }
}
