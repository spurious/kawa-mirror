package kawa.standard;
import gnu.math.*;
import gnu.mapping.ProcedureN;
import gnu.mapping.WrongArguments;

/**
 * Implement the Scheme standard function ">=".
 * @author Per Bothner
 */

public class greaterequal_oper extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    if (args.length < 2)
      throw new WrongArguments (this.name (), 2, "(>= x1 x2 ...)");
    for (int i = 0;  i < args.length - 1;  i++)
      {
	Object arg1 = args[i];
	Object arg2 = args[i+1];
	if (! ((Numeric)arg1).geq (arg2))
	  return Boolean.FALSE;
      }
    return Boolean.TRUE;
  }
}
