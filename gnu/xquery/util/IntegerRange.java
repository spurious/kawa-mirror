// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.math.IntNum;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class IntegerRange extends CpsProcedure // implements Inlineable
{
  public static final IntegerRange integerRange = new IntegerRange("to");

  public IntegerRange(String name)
  {
    super(name);
  }

  public static final IntNum MIN_INT = IntNum.make(Integer.MIN_VALUE);
  public static final IntNum MAX_INT = IntNum.make(Integer.MAX_VALUE);

  /*
  public static void integerRange(int first, int last, Consumer out)
  {
    int step = first > last ? -1 : 1;
    for (;;)
      {
	out.writeInt(first);
	if (first == last)
	  break;
	first += step;
      }
  }
  */

  public static void integerRange(IntNum first, IntNum last, Consumer out)
  {
    if (IntNum.compare(first, last) <= 0)
      {
	if (IntNum.compare(first, MIN_INT) >= 0
	    && IntNum.compare(last, MAX_INT) <= 0)
	  {
	    int fst = first.intValue();
	    int lst = last.intValue();
	    for (;;)
	      {
		out.writeInt(fst);
		if (fst == lst)
		  break;
		fst++;
	      }
	  }
	else
	  {
	    for (;;)
	      {
		out.writeObject(first);
		if (IntNum.compare(first, last) >= 0)
		  break;
		first = IntNum.add(first, 1);
	      }

	  }
      }
    else // first > last:
      {
	if (IntNum.compare(first, MAX_INT) <= 0
	    && IntNum.compare(last, MIN_INT) >= 0)
	  {
	    int fst = first.intValue();
	    int lst = last.intValue();
	    for (;;)
	      {
		out.writeInt(fst);
		if (fst == lst)
		  break;
		fst--;
	      }
	  }
	else
	  {
	    for (;;)
	      {
		out.writeObject(first);
		if (IntNum.compare(first, last) <= 0)
		  break;
		first = IntNum.add(first, -1);
	      }
	  }
      }
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object first = ctx.getNextArg();
    Object last = ctx.getNextArg();
    ctx.lastArg();
    integerRange((IntNum) first, (IntNum) last, ctx.consumer);
  }
}
