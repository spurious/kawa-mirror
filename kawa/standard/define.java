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
	      return new SetExp ((Symbol)p1.car, interp.rewrite (p2.car));
	  }
	else if (p1.car instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.car;
	    if (p2.car instanceof Symbol)
	      return new SetExp ((Symbol) p2.car,
				 new LambdaExp (p2.cdr, p1.cdr, interp));
	  }
      }
    throw new kawa.lang.WrongArguments("define",1,"(define ...)");
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin define>");
  }
}
