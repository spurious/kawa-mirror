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
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return interp.syntaxError ("invalid syntax for if");
    return new IfExp (interp.rewrite (match[0]),
		      interp.rewrite (match[1]),
		      interp.rewrite (match[2]));
  }
}
