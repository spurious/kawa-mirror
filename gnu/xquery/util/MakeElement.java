// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class MakeElement extends CpsProcedure implements Inlineable
{
  public static final MakeElement makeElement = new MakeElement();

  public int numArgs() { return 0xFFFFF001; }

  public void apply (CallContext ctx)
  {
    Object type = ctx.getNextArg();
    String name = type.toString();
    Consumer out = ctx.consumer;
    out.beginGroup(name, type);
    Object endMarker = Special.dfault;
    for (;;)
      {
        Object arg = ctx.getNextArg(endMarker);
	if (arg == endMarker)
	  break;
	if (arg instanceof Consumable)
	  ((Consumable) arg).consume(out);
	else
	  ctx.writeValue(arg);
      }
    out.endGroup(name);
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
	// Stack:  consumer, consumer, type
	code.emitDup();
	code.emitInvokeVirtual(comp.typeObject.getDeclaredMethod("toString", 0));
	// Stack:  consumer, consumer, type, name
	code.emitDup(1, 2); // dup_x2
	// Stack:  consumer, name, consumer, type, name
	code.emitSwap();
	code.emitInvokeInterface(beginGroupMethod);
	// Stack:  consumer, name
	for (int i = 1;  i < nargs;  i++)
	  args[i].compile(comp, target);
	code.emitInvokeInterface(endGroupMethod);
      }
    else if (target instanceof IgnoreTarget)
      ApplyExp.compile(exp, comp, target);
    else
      ConsumerTarget.compileUsingConsumer(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  static final Method beginGroupMethod
    = Compilation.typeConsumer.getDeclaredMethod("beginGroup", 2);
  static final Method endGroupMethod
    = Compilation.typeConsumer.getDeclaredMethod("endGroup", 1);
}
