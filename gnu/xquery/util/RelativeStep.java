// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.math.IntNum;

/** Implements XPath path expression.
 * The XPath expression E1/E2 is compiled into:
 * (relative-step E1 (lambda (dot position last) E2)).
 */

public class RelativeStep extends CpsProcedure
{
  public static final RelativeStep relativeStep = new RelativeStep();

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object arg = ctx.getNextArg();
    Object next = ctx.getNextArg();
    Procedure proc = (Procedure) next;
    Consumer out = ctx.consumer;
    SortedNodes nodes = new SortedNodes();
    ctx.consumer = nodes;
    IntNum countObj;
    if (arg instanceof Values)
      {
	Values values = (Values) arg;
	int count = values.size();
	int it = 0;
	countObj = IntNum.make(count);
	for (int pos = 1; pos <= count; pos++)
	  {
	    it = values.nextPos(it);
	    Object dot = values.getPosPrevious(it);
	    ctx.setArgs(dot, IntNum.make(pos), countObj);
	    proc.apply(ctx);
	    ctx.runUntilDone();
	  }
      }
    else
      {
	countObj = IntNum.one();
	ctx.setArgs(arg, countObj, countObj);
	proc.apply(ctx);
	ctx.runUntilDone();
      }
    nodes.consume(out);
    ctx.consumer = out;
  }
}
