// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
/* BEGIN JAVA2 */
import java.util.Iterator;
/* END JAVA2 */

/* A function that maps an Iterator into the sequence of ite elements. */

public class IteratorItems extends CpsProcedure
{
  public static IteratorItems iteratorItems = new IteratorItems();

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    Object arg = ctx.getNextArg();
    ctx.lastArg();

    /* BEGIN JAVA2 */
    Iterator iter = (Iterator) arg;
    /* END JAVA2 */
    /* BEGIN JAVA1 */
    // SeqPosition iter = (SeqPosition) arg;
    /* END JAVA1 */
    while (iter.hasNext())
      {
	Object val = iter.next();
	Values.writeValues(val, out);
      }
  }
}
