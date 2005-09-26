// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.*;
import gnu.kawa.functions.AddOp;

public class Average extends Procedure1
{
  public static final Average avg = new Average("avg");

  public Average (String name)
  {
    super(name);
  }

  Object combine (Object arg1, Object arg2)
  {
    arg2 = NumberValue.numberValue(arg2);
    if (arg1 == Values.empty)
      return arg2;
    return AddOp.$Pl(NumberValue.numberValue(arg1), arg2);
  }

  public Object apply1(Object arg)
    throws Throwable
  {
    Object sum = Values.empty;
    int count = 0;
    if (arg instanceof Values)
      {
	TreeList tlist = (TreeList) arg;
	int index = 0;
	for (;;)
	  {
	    Object next = tlist.getPosNext(index);
	    if (next == Sequence.eofValue)
	      break;
	    count++;
	    sum = combine(sum, next);
	    index = tlist.nextPos(index);
	  }
      }
    else
      {
	count = 1;
	sum = combine(sum, arg);
      }
    if (sum != Values.empty)
      sum = ((Numeric) sum).div(DFloNum.make(count));
    return sum;
  }
}
