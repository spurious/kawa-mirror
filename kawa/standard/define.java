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
    if (obj instanceof pair)
      {
	pair p1 = (pair) obj;
	if (p1.car instanceof symbol && p1.cdr instanceof pair)
	  {
	    pair p2 = (pair) p1.cdr;
	    if (p2.cdr instanceof snull)
	      return new SetExp ((symbol)p1.car, interp.rewrite (p2.car));
	  }
	else if (p1.car instanceof pair)
	  {
	    pair p2 = (pair) p1.car;
	    if (p2.car instanceof symbol)
	      return new SetExp ((symbol) p2.car,
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
