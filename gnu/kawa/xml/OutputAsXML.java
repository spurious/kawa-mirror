// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;

/** A 1-argument Procedure that takes a value and return output in XML syntax.
 * The result Consumer is typically an OutPort.
 */

public class OutputAsXML extends CpsProcedure
{
   public int numArgs() { return 0x1001; }

    public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object value = ctx.getNextArg();
    ctx.lastArg();
    XMLPrinter out = new XMLPrinter(consumer);
    out.writeObject(value);
    /*
    ctx.consumer = out;
    try
      {
	...;
      }
    finally
      {
	ctx.consumer = consumer;
      }
    */
  }
}
