package kawa.lang;

/**
 * The Syntax transformer that re-writes the lambda builtin.
 * @author	Per Bothner
 */

public class Lambda extends Syntax implements Printable
{
  static private Pattern pattern = new VarListPat (1);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("missing formals in lambda");
    int old_errors = tr.errors;
    Expression result = new LambdaExp (match[0], match[1], tr);
    if (tr.errors > old_errors)
      return new ErrorExp("bad lambda expression");
    return result;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin lambda>");
  }
}
