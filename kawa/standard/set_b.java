package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "set!" primitive.
 * @author	Per Bothner
 */

public class set_b extends Syntax implements Printable
{

  static private Pattern pattern = new ListPat (2, 2);

  public Expression rewrite (Object obj, Interpreter interp)
       throws WrongArguments
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return interp.syntaxError ("missing or extra arguments to set!");
    if (! (match[0] instanceof Symbol))
      return interp.syntaxError ("first set! argument is not a variable name");
    Symbol sym = (Symbol) match[0];
    Expression value = interp.rewrite (match[1]);
    SetExp sexp = new SetExp (sym, value);
    sexp.binding = interp.resolve (sym);
    if (sexp.binding != null)
      sexp.binding.noteValue (value);
    return sexp;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin set!>");
  }
}
