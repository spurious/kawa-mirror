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
  static private Pattern pattern = new ListPat (2, 3, Values.empty);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("invalid syntax for if");
    return new IfExp (tr.rewrite (match[0]),
		      tr.rewrite (match[1]),
		      tr.rewrite (match[2]));
  }
}
