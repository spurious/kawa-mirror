// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.ElementConstructor;

public class MakeElement extends CpsProcedure implements CanInline, Inlineable
{
  public static final MakeElement makeElement = new MakeElement();

  public int numArgs() { return 0xFFFFF001; }

  public static void beginGroup(Consumer out, Object type)
  {
    String name;
    if (type instanceof ElementConstructor)
      {
	ElementConstructor cons = (ElementConstructor) type;
	name = cons.getXmlName();
	type = cons.getQName();
      }
    else if (type instanceof Symbol)
      name = ((Symbol) type).getLocalName();
    else
      name = type.toString();
    out.beginGroup(name, type);
  }

  public static void endGroup(Consumer out, Object type)
  {
    String name;
    if (type instanceof ElementConstructor)
      {
	ElementConstructor cons = (ElementConstructor) type;
	name = cons.getXmlName();
      }
    else if (type instanceof Symbol)
      name = ((Symbol) type).getLocalName();
    else
      name = type.toString();
    out.endGroup(name);
  }

  public void apply (CallContext ctx)
  {
    Object type = ctx.getNextArg();
    Consumer out = ctx.consumer;
    beginGroup(out, type);
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
    endGroup(out, type);
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (nargs > 1 && args[0] instanceof QuoteExp)
      {
	Object tag = ((QuoteExp) args[0]).getValue();
	if (tag instanceof ElementConstructor)
	  {
	    nargs--;
	    Expression[] xargs = new Expression[nargs];
	    System.arraycopy(args, 1, xargs, 0, nargs);
	    return new ApplyExp(args[0], xargs).setLine(exp);
	  }
      }
    return exp;
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
	code.emitDup(1, 1); // dup_x1
	// Stack:  consumer, tagtype, consumer, tagtype
	code.emitInvokeStatic(beginGroupMethod);
	// Stack:  consumer, name
	for (int i = 1;  i < nargs;  i++)
	  args[i].compile(comp, target);
	code.emitInvokeStatic(endGroupMethod);
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

  static final ClassType typeMakeElement
    = ClassType.make("gnu.xquery.util.MakeElement");
  static final Method beginGroupMethod
    = typeMakeElement.getDeclaredMethod("beginGroup", 2);
  static final Method endGroupMethod
    = typeMakeElement.getDeclaredMethod("endGroup", 2);
}
