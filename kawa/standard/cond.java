package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the "cond" Scheme primitive.
 * @author	R. Alexander Milowski
 * @author	Per Bothner
 */

public class cond extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    if (! (obj instanceof Pair))
      return interp.syntaxError ("missing clause for cond");
    Pair clauses = (Pair) obj;
    Object else_part = clauses.cdr;
    if (! (clauses.car instanceof Pair))
      return interp.syntaxError ("mis-formed cond clause");
    Object if_part = ((Pair)clauses.car).car;
    Object then_part = ((Pair)clauses.car).cdr;

    if (if_part instanceof Symbol
	&& ((Symbol)if_part).toString().equalsIgnoreCase("else"))
      {
	if (else_part != Interpreter.nullObject)
	  return interp.syntaxError ("else clause must be last clause of cond");
	return interp.rewrite_body (then_part);
      }

    if (then_part == Interpreter.nullObject)
      {
	if (else_part == Interpreter.nullObject)
	  return interp.rewrite (if_part);
	// later:  use code from the implementation of or.  FIXME
	// I.e. handle this case as:  (or if_part (cond else_part))
	return interp.syntaxError ("not implemented:  cond with singleton clause");
      }

    // Note we use recursion to handle the remaining clauses.
    return new IfExp (interp.rewrite (if_part),
		      interp.rewrite_body (then_part),
		      else_part == Interpreter.nullObject ? null
		      : rewrite (else_part, interp));
  }
}
