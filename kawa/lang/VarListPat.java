package kawa.lang;

/** A Pattern that will match a list of a given minimum length. */

public class VarListPat extends Pattern {
  /** Minimun length of list that will match. */
  int min_length;

  public VarListPat (int min) { min_length = min; }

  /** Succeeds of obj is a list with at least min_length elements.
   * @param obj the object to match against
   * @return null on failure; on success an array of min_length+1 elements
   * where the first min_length elements are from obj, and the last
   * element is the min_length cdr of obj. */
  public Object[] match (Object obj)
  {
    Object[] result = new Object[min_length + 1];
    int i;
    for (i = 0; i < min_length; i++)
      {
	if (obj instanceof kawa.lang.pair)
	  {
	    kawa.lang.pair p = (kawa.lang.pair)obj;
	    result[i] = p.car;
	    obj = p.cdr;
	  }
	else
	  return null;
      }
    result[i] = obj;
    return result;
  }
}
