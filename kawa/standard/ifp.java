package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the "if" Scheme primitive.
 * @author	Per Bothner
 */

public class ifp extends Syntax implements Printable
{
  static private Pattern pattern = new ListPat (2, 3);

  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      throw new kawa.lang.WrongArguments("if",2,"(if test exp [exp])");
    return new IfExp (interp.rewrite (match[0]),
		      interp.rewrite (match[1]),
		      interp.rewrite (match[2]));
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin if>");
  }
}
