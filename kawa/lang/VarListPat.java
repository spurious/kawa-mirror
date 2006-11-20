package kawa.lang;
import gnu.lists.*;
import gnu.text.Printable;

/** A Pattern that will match a list of a given minimum length. */

public class VarListPat extends Pattern {
  /** Minimun length of list that will match. */
  int min_length;

  public VarListPat (int min) { min_length = min; }

  /** Succeeds of obj is a list with at least min_length elements.
   * @param obj the object to match against
   * @return true if the match succeeded.
   * The elements vars[start_vars .. start_vars + min_length] contain
   * the first min_length elements of obj followed by the
   * min_length'th cdr of obj. */
  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    int i;
    for (i = 0; i < min_length; i++)
      {
	if (obj instanceof Pair)
	  {
	    Pair p = (Pair)obj;
	    vars[start_vars + i] = p.car;
	    obj = p.cdr;
	  }
	else
	  return false;
      }
    vars [start_vars + i] = obj;
    return true;
  }

  public int varCount () { return min_length + 1; }

  public void print (Consumer out)
  {
    out.append("#<varlist-pattern min:");
    out.append(Integer.toString(min_length));
    out.append('>');
  }
}
