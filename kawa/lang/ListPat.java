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
   * @return -1 on failure;  max_vars on success
   * On success, max_length values from the elements of the list are placed
   * in vars (starting at start_vars); if obj is shorter, missing elements
   * are set to default_value.
   */
  public int match (Object obj, Object[] vars, int start_vars)
  {
    int i;
    for (i = 0; i < max_length; i++)
      {
	if (obj instanceof Pair)
	  {
	    Pair p = (Pair)obj;
	    vars [start_vars + i] = p.car;
	    obj = p.cdr;
	  }
	else if (i < min_length)
	  return -1;
	else
	  break;
      }
    for ( ; i < max_length; i++)
      vars [start_vars + i] = default_value;
    return max_length;
  }

  public int varCount () { return max_length; }
}
