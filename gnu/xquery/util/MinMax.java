// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;

public class MinMax extends Procedure1
{
  public static final MinMax min = new MinMax("min", false);
  public static final MinMax max = new MinMax("max", true);

  boolean returnMax;

  public MinMax(String name, boolean returnMax)
  {
    super(name);
    this.returnMax = returnMax;
  }

  public static Object minmax(Object arg1, Object arg2, boolean returnMax)
  {
    if (arg1 == Values.empty)
      return arg2; // FIXME - verify that arg2 is comparable.
    int flags = returnMax ? Compare.TRUE_IF_GRT :  Compare.TRUE_IF_LSS;
    return Compare.apply(flags, arg1, arg2) ? arg1 : arg2;
  }

  public Object apply1(Object arg)
  {
    Object result = Values.empty;
    if (arg instanceof Values)
      {
	TreeList tlist = (TreeList) arg;
	int index = 0;
	for (;;)
	  {
	    Object next = tlist.getNext(index << 1, null);
	    if (next == Sequence.eofValue)
	      return result;
	    result = minmax(result, next, returnMax);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      return minmax(result, arg, returnMax);
  }
}
