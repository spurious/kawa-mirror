// Copyright (c) 2003  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xquery.util.StringValue;  // FIXME bad dependency

public class TextConstructor extends CpsProcedure
{
  public static final TextConstructor textConstructor = new TextConstructor();

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    int nargs = ctx.count;
    StringBuffer sbuf = new StringBuffer();
    for (int i = 0;  i < nargs;  i++)
      {
	Object arg = ctx.getArgAsObject(i);
	StringValue.stringValue(arg, sbuf);
	out.writeChars(sbuf.toString());
	sbuf.setLength(0);
      }
  }
}
