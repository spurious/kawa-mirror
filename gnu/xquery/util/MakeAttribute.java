// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class MakeAttribute extends CpsProcedure implements Inlineable
{
  public static final MakeAttribute makeAttribute = new MakeAttribute();

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx)
  {
    Object type = ctx.getNextArg();
    String name = type.toString();
    Consumer out = ctx.consumer;
    out.beginAttribute(name, type);
    Object arg = ctx.getNextArg();
    if (arg instanceof Consumable)
      ((Consumable) arg).consume(out);
    else
      ctx.writeValue(arg);
    ctx.lastArg();
    out.endAttribute();
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    if (target instanceof ConsumerTarget)
      {
	Variable consumer = ((ConsumerTarget) target).getConsumerVariable();
	Expression[] args = exp.getArgs();
	int nargs = args.length;
	CodeAttr code = comp.getCode();
	
	code.emitLoad(consumer);
	code.emitDup();
	args[0].compile(comp, Target.pushObject);
	code.emitDup();
	code.emitInvokeVirtual(Type.pointer_type.getDeclaredMethod("toString", 0));
	code.emitSwap();
	code.emitInvokeInterface(beginAttributeMethod);
	for (int i = 1;  i < nargs;  i++)
	  args[i].compile(comp, target);
	code.emitInvokeInterface(endAttributeMethod);
      }
    else if (target instanceof IgnoreTarget)
      ApplyExp.compile(exp, comp, target);
    else
      ConsumerTarget.compileUsingConsumer(exp, comp, target);
  }

  static final Method beginAttributeMethod
    = Compilation.typeConsumer.getDeclaredMethod("beginAttribute", 2);
  static final Method endAttributeMethod
    = Compilation.typeConsumer.getDeclaredMethod("endAttribute", 0);

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }
}
