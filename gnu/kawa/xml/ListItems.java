// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
/* BEGIN JAVA2 */
import java.util.Iterator;
import java.util.List;
/* END JAVA2 */

/* A function that maps a List into the sequence of ite elements. */

public class ListItems extends CpsProcedure
{
  public static ListItems listItems = new ListItems();

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    Object arg = ctx.getNextArg();
    ctx.lastArg();

    /* BEGIN JAVA2 */
    List list = (List) arg;
    if (arg instanceof AbstractSequence)
      {
	((AbstractSequence) arg).consumePosRange(0, -1, out);
	return;
      }
    Iterator iter = list.iterator();
    while (iter.hasNext())
      {
	Object val = iter.next();
	Values.writeValues(val, out);
      }
    /* END JAVA2 */
    /* BEGIN JAVA1 */
    // ((AbstractSequence) arg).consumePosRange(0, -1, out);
    /* END JAVA1 */
  }
}
