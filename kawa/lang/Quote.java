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
    obj = pair.car;
    if (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).form;
    /*
    Doing namespaceResolve is probably desirable, but need to be recursive.
    if (val instanceof String)
      val = tr.namespaceResolve((String) val);
    */
    return new QuoteExp(obj);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin quote>");
  }
}
