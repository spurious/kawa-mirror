// Copyright (c) 2003, 2004  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xquery.util.StringValue;  // FIXME bad dependency

public class MakeText extends CpsProcedure // NodeConstructor
{
  public static final MakeText makeText = new MakeText();

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = NodeConstructor.pushNodeContext(ctx);
    try
      {
	StringBuffer sbuf = new StringBuffer();
	Object endMarker = Symbol.UNBOUND;
	for (;;)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
	    StringValue.stringValue(arg, sbuf);
	    out.writeChars(sbuf.toString());
	    sbuf.setLength(0);
	  }
      }
    finally
      {
	NodeConstructor.popNodeContext(saved, ctx);
      }
  }

  /*
  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    Variable consumer = target.getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    for (int i = 0;  i < nargs;  i++)
      // FIXME needs to coerce to string value.
      compileChild(args[i], comp, target);
  }
  */
}
