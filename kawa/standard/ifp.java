package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

/**
 * The Syntax transformer that re-writes the "if" Scheme primitive.
 * @author	Per Bothner
 */

public class ifp extends Syntax implements Printable
{
  /** Whether extra else expressions are allowed. */
  boolean allowElseList;

  public ifp ()
  {
  }

  public ifp (String name, boolean allowElseList)
  {
    super(name);
    this.allowElseList = allowElseList;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (obj instanceof Pair)
      {
	Pair p = (Pair) obj;
	Expression test = tr.rewrite(p.car);
	if ((obj = p.cdr) instanceof Pair)
	  {
	    p = (Pair) obj;
	    obj = p.cdr;
	    Expression then_clause = tr.rewrite(p.car);
	    Expression else_clause;
	    if (obj == LList.Empty)
	      else_clause = null;
	    else if (obj instanceof Pair
		     && (p = (Pair) obj).cdr == LList.Empty)
	      else_clause = tr.rewrite(p.car);
	    else if (allowElseList)
	      else_clause = tr.rewrite_body(obj);
	    else
	      return tr.syntaxError("too many expressions for 'if'");
	    return new IfExp(test, then_clause, else_clause);
	  }
      }
    return tr.syntaxError ("missing expressions for 'if'");
  }
}
