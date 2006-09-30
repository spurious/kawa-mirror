// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;

public class Reduce extends Procedure1
{
  public static final Reduce sum = new Reduce("sum", ArithOp.add);

  protected Procedure combiner;

  public Reduce (String name, Procedure combiner)
  {
    super(name);
    this.combiner = combiner;
  }

  public Object combine(Object arg1, Object arg2)
    throws Throwable
  {
    if (arg1 == Values.empty)
      return arg2; // FIXME - verify that arg2 is appropriate;
    return combiner.apply2(arg1, arg2);
  }

  public Object apply1(Object arg)
    throws Throwable
  {
    Object result = Values.empty;
    if (arg instanceof Values)
      {
	TreeList tlist = (TreeList) arg;
	int pos = 0;
	for (;;)
	  {
	    Object next = tlist.getPosNext(pos);
	    if (next == Sequence.eofValue)
	      return result;
	    result = combine(result, next);
            pos = tlist.nextPos(pos);
	  }
      }
    else
      return combine(result, arg);
  }
}
