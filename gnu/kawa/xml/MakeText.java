// Copyright (c) 2003, 2004  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.xml.*;

public class MakeText extends NodeConstructor
{
  public static final MakeText makeText = new MakeText();

  public int numArgs() { return 0x1001; }

  public Object apply1 (Object arg)
  {
    if (arg == null || (arg instanceof Values && ((Values) arg).isEmpty()))
      return arg;
    NodeTree node = new NodeTree();
    text$C(arg, new XMLFilter(node));
    return KText.make(node);
  }

  public static void text$C (Object arg, Consumer out)
  {
    if (arg == null || (arg instanceof Values && ((Values) arg).isEmpty()))
      return;
    String str;
    if (arg instanceof String)
      str = (String) arg;
    else
      {
        StringBuffer sbuf = new StringBuffer();
        if (arg instanceof Values)
          {
            Object[] vals = ((Values) arg).getValues();
            for (int i = 0;  i < vals.length; i++)
              {
                if (i > 0)
                  sbuf.append(' ');
                StringValue.stringValue(vals[i], sbuf);
              }
          }
        else
          StringValue.stringValue(arg, sbuf);
        str = sbuf.toString();
      }
    out.append(str);
  }

  public static void text$X (Object arg, CallContext ctx)
  {
    if (arg == null || (arg instanceof Values && ((Values) arg).isEmpty()))
      return;
    Consumer saved = ctx.consumer;
    Consumer out = NodeConstructor.pushNodeContext(ctx);
    try
      {
        text$C(arg, out);
      }
    finally
      {
	NodeConstructor.popNodeContext(saved, ctx);
      }
  }

  public void apply (CallContext ctx)
  {
    text$X(ctx.getNextArg(null), ctx);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    // We can't use NodeConstructor's compile method, because a node
    // constructor may return a non-node when given an empty sequence.  Sigh.
    ApplyExp.compile(exp, comp, target);
  }

  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    // This only gets called via NodeConstructor's compileChild.
    CodeAttr code = comp.getCode();
    Expression[] args = exp.getArgs();
    args[0].compile(comp, Target.pushObject);
    code.emitLoad(target.getConsumerVariable());
    code.emitInvokeStatic(ClassType.make("gnu.kawa.xml.MakeText")
                          .getDeclaredMethod("text$C", 2));
  }
}
