package kawa.lang;
import gnu.mapping.Printable;
import gnu.expr.*;
import gnu.lists.*;

/**
 * The Syntax transformer that re-writes the "quote" Scheme primitive.
 * @author	Per Bothner
 */

public class Quote extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    Pair pair;
    if (! (obj instanceof Pair)
        || (pair = (Pair) obj).cdr != LList.Empty)
      return tr.syntaxError ("quote requires a single argument");
    return new QuoteExp (pair.car);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin quote>");
  }
}
