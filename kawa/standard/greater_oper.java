package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/**
 * Implement the Scheme standard function ">".
 * @author Per Bothner
 */

public class greater_oper extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length < 2)
      throw new kawa.lang.WrongArguments (this.name (), 2, "(> x1 x2 ...)");
    for (int i = 0;  i < args.length - 1;  i++)
      {
	Object arg1 = args[i];
	Object arg2 = args[i+1];
	if (! ((Numeric)arg1).grt (arg2))
	  return Interpreter.falseObject;
      }
    return Interpreter.trueObject;
  }
}
