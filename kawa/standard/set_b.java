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
    if (match == null || ! (match[0] instanceof Symbol))
      throw new WrongArguments("set!",2,"(set! variable expression)");
    Symbol sym = (Symbol) match[0];
    SetExp sexp = new SetExp (sym, interp.rewrite (match[1]));
    Declaration decl = (Declaration) interp.current_decls.get (sym);
    if (decl != null)
      sexp.binding = decl;
    return sexp;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin set!>");
  }
}
