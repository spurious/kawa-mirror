// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;

/** Map a unary function over a value sequence, yielding a new sequence.
 * Used to implement XQuery's 'for' form.
 */

public class ValuesMap extends CpsProcedure
{
  public static final ValuesMap valuesMap = new ValuesMap();

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx)
  {
    Procedure proc = (Procedure) ctx.getNextArg();
    Consumer out = ctx.consumer;
    Object val = ctx.getNextArg();
    Procedure.checkArgCount(proc, 1);
    if (val instanceof Values)
      {
	int ipos = 0;
	Values values = (Values) val;
	for (;;)
	  {
	    Object v = values.getNext(ipos, null);
	    if (v == Sequence.eofValue)
	      break;
	    ctx.setArgs(v);
	    ctx.proc = proc;
	    ctx.run();
	    ipos = values.nextDataIndex(ipos >> 1);
	    /*
	    if (ipos < 0)
	      break;
	    */
	    ipos = ipos << 1;
	  }
      }
    else
      {
	ctx.setArgs(val);
	ctx.proc = proc;
	ctx.run();
      }
  }
}
