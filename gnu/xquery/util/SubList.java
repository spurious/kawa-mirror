// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;

/** Extracts a sub-range from a value sequence.
 * Implements XQuery 'sublist'. */

public class SubList extends MethodProc
{
  public static final SubList subList = new SubList();

  public int numArgs() { return 0x3002; }

  public static void subList(Object seq, double start, double end,
                             Consumer out)
  {
    if (seq instanceof Values)
      {
	Values vals = (Values) seq;
        int n = 0;
	int i = 0;
	while (++n < start)
	  {
	    i = vals.nextDataIndex(i);
	    if (i < 0)
	      return;
	  }
	int startPosition = i;
	int endPosition = i;
	while (n++ < end)
	  {
	    i = vals.nextDataIndex(i);
	    if (i < 0)
	      break;
	    endPosition = i;
	  }
	vals.consumeIRange(startPosition, endPosition, out);
      }
    else
      {
	if (start <= 1 && end >= 2)
	  out.writeObject(seq);
      }
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object seq = ctx.getNextArg();
    double d1 = Math.round(StringUtils.asDouble(ctx.getNextArg()));
    Object eof = Sequence.eofValue; 
    Object arg2 = ctx.getNextArg(eof);
    ctx.lastArg();
    double d2 = arg2 != eof ? Math.round(StringUtils.asDouble(arg2))
      : Double.POSITIVE_INFINITY;
    subList(seq, d1, d1 + d2, consumer);
  }
}
