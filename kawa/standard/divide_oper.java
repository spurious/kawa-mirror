package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "/".
 * @author Per Bothner
 */

public class divide_oper extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    double dval = ((Number)args[0]).doubleValue ();
    if (args.length == 1)
      return new Double (1.0 / dval);
    for (int i = 0; i < args.length; i++)
      {
	Number arg = (Number) args[i];
	dval = dval / arg.doubleValue ();
      }
    return new Double(dval);
   }
}
