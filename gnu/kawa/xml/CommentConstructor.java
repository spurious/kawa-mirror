// Copyright (c) 2004  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xquery.util.StringValue;  // FIXME bad dependency

public class CommentConstructor extends CpsProcedure // NodeConstructor
{
  public static final CommentConstructor commentConstructor
    = new CommentConstructor();

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    XConsumer out = NodeConstructor.pushNodeContext(ctx);
    try
      {
	StringBuffer sbuf = new StringBuffer();
	Object endMarker = Symbol.UNBOUND;
	for (int i = 0;; i++)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
	    if (i > 0)
	      sbuf.append(' ');
	    StringValue.stringValue(arg, sbuf);
	  }
	int len = sbuf.length();
	char[] buf = new char[len];
	sbuf.getChars(0, len, buf, 0);
	out.writeComment(buf, 0, len);
      }
    finally
      {
	NodeConstructor.popNodeContext(saved, ctx);
      }
  }
}
