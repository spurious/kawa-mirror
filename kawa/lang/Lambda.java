package kawa.lang;

/**
 * The Syntax transformer that re-writes the lambda builtin.
 * @author	Per Bothner
 */

public class Lambda extends Syntax implements Printable
{
  static private Pattern pattern = new VarListPat (1);

  public Expression rewrite (Object obj, Interpreter interp)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return interp.syntaxError ("missing formals in lambda");
    return new LambdaExp (match[0], match[1], interp);
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin lambda>");
  }
}
