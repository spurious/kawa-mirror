package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "-".
 * @author Per Bothner
 */

public class minus_oper extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int ival = 0;
    double dval = 0.0;
    boolean isInteger = true;
    int i = 0;
    if (args.length > 1)
      {
	Object arg = args[0];
	if (arg instanceof Double)
	  {
	    isInteger = false;
            dval = ((Double)arg).doubleValue();
	  }
	else if (arg instanceof Integer)
	  ival = ((Integer)arg).intValue();
	else
	  throw new WrongType(this.name, 1,"number");
	i++;
      }
    for (; i < args.length; i++)
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
