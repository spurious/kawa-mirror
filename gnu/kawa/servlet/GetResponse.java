// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;

/** A 0-argument function that returns the current ServletResponse. */

public class GetResponse extends CpsProcedure
{
  public static final GetResponse getResponse = new GetResponse();

  public int numArgs() { return 0; }

  public void apply (CallContext ctx)
  {
    ctx.lastArg();
    ctx.consumer.writeObject(((ServletCallContext) ctx).response);
  }
}
