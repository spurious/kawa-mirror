// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class MakeProcInst extends NodeConstructor
{
  public static final MakeProcInst makeProcInst
    = new MakeProcInst();

  public int numArgs() { return 0x2002; }

  public static void procInst$C (Object target, Object content, Consumer out)
  {
    target = KNode.atomicValue(target);
    if (! (target instanceof String || target instanceof UntypedAtomic))
      throw new ClassCastException("invalid type of processing-instruction target [XPTY0004]");
    String tname = target.toString();
    if (tname.toLowerCase().startsWith("xml"))
      throw new IllegalArgumentException("processing-instruction target starts with 'xml' [XQDY0064]");
    if (tname.indexOf(':') >= 0) // Actually should check NCName format.  FIXME
      throw new IllegalArgumentException("processing-instruction target is not a NCName [XQDY0041]");
      
    if (! (out instanceof XConsumer))
      return;
    StringBuffer sbuf = new StringBuffer();
    if (content instanceof Values)
      {
        Object[] vals = ((Values) content).getValues();
        for (int i = 0;  i < vals.length; i++)
          {
            if (i > 0)
              sbuf.append(' ');
            StringValue.stringValue(vals[i], sbuf);
          }
      }
    else
      StringValue.stringValue(content, sbuf);
    int length = sbuf.length();
    int start = 0;
    while (start < length && Character.isWhitespace(sbuf.charAt(start)))
      start++;
    char[] chars = new char[length-start];
    sbuf.getChars(start, length, chars, 0);
    ((XConsumer) out).writeProcessingInstruction(tname,
                                                 chars, 0, chars.length);
  }

  public static void procInst$X (Object target, Object content,
                                     CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = NodeConstructor.pushNodeContext(ctx);
    try
      {
        procInst$C(target, content, out);
      }
    finally
      {
	NodeConstructor.popNodeContext(saved, ctx);
      }
  }

  public void apply (CallContext ctx)
  {
    procInst$X(ctx.getNextArg(null), ctx.getNextArg(null), ctx);
  }

  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    CodeAttr code = comp.getCode();
    Expression[] args = exp.getArgs();
    args[0].compile(comp, Target.pushObject);
    args[1].compile(comp, Target.pushObject);
    code.emitLoad(target.getConsumerVariable());
    code.emitInvokeStatic(ClassType.make("gnu.kawa.xml.MakeProcInst")
                          .getDeclaredMethod("procInst$C", 3));
  }
}
