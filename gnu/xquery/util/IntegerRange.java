// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
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

  public void integerRange(IntNum first, IntNum last, Consumer out)
  {
    if (IntNum.compare(first, MIN_INT) >= 0
	&& IntNum.compare(last, MAX_INT) <= 0)
      {
	int fst = first.intValue();
	int lst = last.intValue();
	if (fst <= lst)
	  {
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
		out.writeInt(lst);
		if (fst == lst)
		  break;
		lst--;
	      }
	  }
      }
    else if (IntNum.compare(first, last) <= 0)
      {
	while (IntNum.compare(first, last) <= 0)
	  {
	    out.writeObject(first);
	    first = IntNum.add(first, 1);
	  }
      }
    else
      {
	while (IntNum.compare(first, last) <= 0)
	  {
	    out.writeObject(last);
	    last = IntNum.add(last, -1);
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
