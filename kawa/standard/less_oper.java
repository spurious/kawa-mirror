package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "<".
 * @author Per Bothner
 */

public class less_oper extends ProcedureN
{
  public less_oper()
  {
    super("<");
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length < 2)
      throw new kawa.lang.WrongArguments(this.name(),2,"(< x1 x2 ...)");
    for (int i = 0;  i < args.length - 1;  i++)
      {
	Object arg1 = args[i];
	Object arg2 = args[i+1];
	if (! (arg1 instanceof Number))
	  throw new kawa.lang.WrongType(name (), i, "number");
	if (! (arg2 instanceof Number))
	  throw new kawa.lang.WrongType(name (), i+1, "number");
	if (arg1 instanceof Integer && arg2 instanceof Integer)
	  {
	    int int1 = ((Integer)arg1).intValue ();
	    int int2 = ((Integer)arg2).intValue ();
	    if (! (int1 < int2))
	      return Interpreter.falseObject;
	  }
	else
	  {
	    double double1 = ((Number)arg1).doubleValue ();
	    double double2 = ((Number)arg2).doubleValue ();
	    if (! (double1 < double2))
	      return Interpreter.falseObject;
	  }
      }
    return Interpreter.trueObject;
  }
}
