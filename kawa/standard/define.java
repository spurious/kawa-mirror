package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the "define" Scheme primitive.
 * Currently, only handles top-level definitions.
 * @author	Per Bothner
 */

public class define extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof Symbol && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == List.Empty)
	      {
		SetExp result = new SetExp ((Symbol)p1.car,
					    interp.rewrite (p2.car));
		result.setDefining (true);
		return result;
	      }
	  }
	else if (p1.car instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.car;
	    if (p2.car instanceof Symbol)
	      {
		Symbol name = (Symbol) p2.car;
		LambdaExp lexp = new LambdaExp (p2.cdr, p1.cdr, interp);
		lexp.setName (name.toString ());
		SetExp result = new SetExp (name, lexp);
		result.setDefining (true);
		return result;
	      }
	  }
      }
    return interp.syntaxError ("invalid syntax for define");
  }
}
