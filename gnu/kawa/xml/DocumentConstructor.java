// Copyright (c) 2003  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

public class DocumentConstructor extends CpsProcedure
{
  public static final DocumentConstructor documentConstructor
    = new DocumentConstructor();

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    Object endMarker = Symbol.UNBOUND;
    out.beginDocument();
    for (;;)
      {
	Object arg = ctx.getNextArg(endMarker);
	if (arg == endMarker)
	  break;
	if (arg instanceof Consumable)
	  ((Consumable) arg).consume(out);
	else
	  out.writeObject(arg);
      }
    out.endDocument();
  }
}
