package kawa.lang;
import gnu.mapping.Printable;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the "quote" Scheme primitive.
 * @author	Per Bothner
 */

public class Quote extends Syntax implements Printable
{
  static private Pattern pattern = new ListPat (1);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("quote requires a single argument");
    return new QuoteExp (match[0]);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin quote>");
  }
}
