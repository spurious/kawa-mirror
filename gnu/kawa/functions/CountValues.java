// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.lists.Consumer;

/** Return the number of values in the argument. */

public class CountValues extends Procedure1
{
  public static final CountValues countValues = new CountValues();

  public static int countValues(Object arg)
  {
    return arg instanceof Values ? ((Values) arg).size() : 1;
  }

  public Object apply1(Object arg)
  {
    return gnu.math.IntNum.make(countValues(arg));
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object arg = ctx.getNextArg();
    ctx.lastArg();
    consumer.writeInt(countValues(arg));
  }
}

