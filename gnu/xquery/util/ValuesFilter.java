// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class ValuesFilter extends CpsProcedure
{
  public int numArgs() { return 0x2002; }

  static public boolean matches(Object result, long count)
  {
    if (result instanceof Boolean)
      return ((Boolean) result).booleanValue();
    if (result instanceof Number)
      return count == ((Number) result).longValue();
    if (result instanceof SeqPosition)
      return true;
    if (result instanceof Values)
      {
	Values values = (Values) result;
	int index = 0;
	for (;;)
	  {
	    int next = values.nextDataIndex(index);
	    if (next < 0)
	      return false;
	    if (matches(values.getNext(index << 1, null), count))
	      return true;
	    index = next;
	  }
      }
    if (result instanceof TreeList)
      return ! ((TreeList) result).isEmpty();
    throw new Error("unimplemented condition type"); // FIXME
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object values = ctx.getNextArg();
    Procedure proc = (Procedure) ctx.getNextArg();
    long count = 0;
    int index = 0;
    Consumer out = ctx.consumer;
    for (;;)
      {
	int next = Values.nextIndex(values, index);
	if (next < 0)
	  break;
	Object value = Values.nextValue(values, index);
	count++;
	if (matches(proc.apply1(value), count))
	  out.writeObject(value);
	index = next;
      }
  }

  public static final ValuesFilter valuesFilter = new ValuesFilter();
}
