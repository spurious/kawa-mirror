package kawa.lang;

/** Match a list whose length in in the range [min_length .. max_length]. */

public class ListPat extends Pattern
{
  /** Minimun length of list that will match. */
  int min_length;
  /** Maximum length of list that will match. */
  int max_length;
  Object default_value;

  public ListPat (int len) { min_length = len;  max_length = len; }
  public ListPat (int min, int max) { min_length = min;  max_length = max; }
  public ListPat (int min, int max, Object default_val)
  { min_length = min;  max_length = max; default_value = default_val; }

  /**
   * Succeeds if obj is a list of length [min_length..max_length].
   * @param obj the object to match against
   * @return null on failure; on success, an array of the elements of the list.
   * The result has length max_length; if obj is shorter, missing elements
   * are set to default_value. */
  public Object[] match (Object obj)
  {
    Object[] result = new Object[max_length];
    int i;
    for (i = 0; i < max_length; i++)
      {
	if (obj instanceof Pair)
	  {
	    Pair p = (Pair)obj;
	    result[i] = p.car;
	    obj = p.cdr;
	  }
	else if (i < min_length)
	  return null;
	else
	  break;
      }
    for ( ; i < max_length; i++)
      result[i] = default_value;
    return result;
  }
}
