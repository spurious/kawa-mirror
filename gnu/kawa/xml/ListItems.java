// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import java.util.Iterator;
import java.util.List;

/* A function that maps a List into the sequence of ite elements. */

public class ListItems extends CpsProcedure
{
  public static ListItems listItems = new ListItems();

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    Object arg = ctx.getNextArg();
    ctx.lastArg();

    List list = (List) arg;
    // FIXME - should just 'consume' the list.
    Iterator iter = list.iterator();
    while (iter.hasNext())
      {
	Object val = iter.next();
	Values.writeValues(val, out);
      }
  }
}
