// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import java.util.Iterator;

/* A function that maps an Iterator into the sequence of ite elements. */

public class IteratorItems extends CpsProcedure
{
  public static IteratorItems iteratorItems = new IteratorItems();

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    Object arg = ctx.getNextArg();
    ctx.lastArg();

    Iterator iter = (Iterator) arg;
    while (iter.hasNext())
      {
	Object val = iter.next();
	Values.writeValues(val, out);
      }
  }
}
