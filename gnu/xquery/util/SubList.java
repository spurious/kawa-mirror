// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;

/** Extracts a sub-range from a value sequence.
 * Implements XQuery 'sublist'. */

public class SubList extends CpsProcedure
{
  public static final SubList subList = new SubList();

  public int numArgs() { return 0x3002; }

  public static void subList(Object seq, int first, int length, Consumer out)
  {
    first--;
    if (seq instanceof Values)
      {
	Values vals = (Values) seq;
	int i = 0;
	while (--first >= 0)
	  {
	    i = vals.nextDataIndex(i);
	    if (i < 0)
	      return;
	  }
	int startPosition = i;
	int endPosition = i;
	while (--length >= 0)
	  {
	    i = vals.nextDataIndex(i);
	    if (i < 0)
	      break;
	    endPosition = i;
	  }
	vals.consumeRange(startPosition, endPosition, out);
      }
    else
      {
	if (length > 0 && first == 0)
	  out.writeObject(seq);
      }
  }

  public static Object subList(Object seq, int first, int length)
  {
    Values vals = new Values();
    subList(seq, first, length, vals);
    int count  = vals.size();
    if (count == 0)
      return Values.empty;
    if (count == 1)
      return vals.get(0);
    return vals;
  }

  public static Object subList(Object seq, int first)
  {
    return subList(seq, first, Integer.MAX_VALUE);
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object seq = ctx.getNextArg();
    int first = ctx.getNextIntArg();
    int length = ctx.getNextIntArg(Integer.MAX_VALUE);
    ctx.lastArg();
    subList(seq, first, length, consumer);
  }
}
