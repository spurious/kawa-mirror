package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "-".
 * @author Per Bothner
 */

public class minus_oper extends ProcedureN
{
  public minus_oper()
  {
    super("-");
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int ival = 0;
    double dval = 0.0;
    boolean isInteger = true;
    for (int i = 0; i < args.length; i++)
      {
	Object arg = args[i];
	if (arg instanceof Double)
	  {
            if (isInteger)
	      {
		isInteger = false;
		dval = ival;
	      }
            dval -= ((Double)arg).doubleValue();
	  }
	else if (arg instanceof Integer)
	  {
            if (isInteger)
	      ival -= ((Integer)arg).intValue();
	    else
	      dval -= ((Integer)arg).intValue();
	  }
	else
	  throw new WrongType(this.name,i + 1,"number");
      }

      if (isInteger)
	return new Integer(ival);
      else
	return new Double(dval);
   }
}
