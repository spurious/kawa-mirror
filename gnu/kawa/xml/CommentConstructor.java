// Copyright (c) 2004  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

public class CommentConstructor extends MethodProc // NodeConstructor
{
  public static final CommentConstructor commentConstructor
    = new CommentConstructor();

  public int numArgs() { return 0x1001; }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    XConsumer out = NodeConstructor.pushNodeContext(ctx);
    try
      {
	StringBuffer sbuf = new StringBuffer();
	Object endMarker = Location.UNBOUND;
	for (int i = 0;; i++)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
	    if (i > 0)
	      sbuf.append(' ');
            if (arg instanceof Values)
              {
                Values vals = (Values) arg;
                for (int it = 0;  (it = vals.nextPos(it)) != 0; )
                  StringValue.stringValue(vals.getPosPrevious(it), sbuf);
              }
            else
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
