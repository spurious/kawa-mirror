// Copyright (c) 2003  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;

/** Used to implement 'some - satisfies' and 'every - satisfies'.
 * A 2-argument Procedure (similar to ValuesMap), where the first
 * argument is a Procedure that maps a value to a boolean, and
 * the second argument is a sequence of values to pass to the former.
 */

public class ValuesEvery extends CpsProcedure
{
  public static final ValuesEvery every = new ValuesEvery(true);
  public static final ValuesEvery some = new ValuesEvery(false);

  public ValuesEvery(boolean matchAll) {this.matchAll = matchAll; }

  /** True for every, false for some. */
  boolean matchAll;

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Procedure proc = (Procedure) ctx.getNextArg();
    Consumer out = ctx.consumer;
    Object val = ctx.getNextArg();
    boolean ok = matchAll;
    Procedure.checkArgCount(proc, 1);
    if (val instanceof Values)
      {
	int ipos = 0;
	Values values = (Values) val;
	while ((ipos = values.nextPos(ipos)) != 0)
	  {
	    ctx.setArgs(values.getPosPrevious(ipos));
	    ctx.proc = proc;
	    ok = BooleanValue.booleanValue(ctx.runUntilValue());
	    if (ok != matchAll)
	      break;
	  }
      }
    else
      {
	ctx.setArgs(val);
	ctx.proc = proc;
	ok = BooleanValue.booleanValue(ctx.runUntilValue());
      }
    ctx.consumer.writeBoolean(ok);
  }

}
