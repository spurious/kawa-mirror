package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the "if" Scheme primitive.
 * @author	Per Bothner
 */

public class ifp extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = ListPat.match(2, 3, Values.empty, obj);
    if (match == null)
      return tr.syntaxError ("invalid syntax for if");
    return new IfExp (tr.rewrite (match[0]),
		      tr.rewrite (match[1]),
		      tr.rewrite (match[2]));
  }
}
