// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.kawa.xml.*;

public class MakeAttribute extends CpsProcedure implements Inlineable
{
  public static final MakeAttribute makeAttribute = new MakeAttribute();

  public int numArgs() { return 0xFFFFF001; }

  public static void beginAttribute(Consumer out, Object type)
  {
    String name;
    if (type instanceof AttributeConstructor)
      {
	AttributeConstructor cons = (AttributeConstructor) type;
	name = cons.getXmlName();
	type = cons.getQName();
      }
    else if (type instanceof ElementConstructor)
      {
	ElementConstructor cons = (ElementConstructor) type;
	name = cons.getXmlName();
	type = cons.getQName();
      }
    else if (type instanceof Symbol)
      name = ((Symbol) type).getLocalName();
    else
      name = type.toString();
    out.beginAttribute(name, type);
  }

  public void apply (CallContext ctx)
  {
    Object type = ctx.getNextArg();
    Consumer out = ctx.consumer;
    beginAttribute(out, type);
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
	// Stack:  consumer, consumer, tagtype
	code.emitInvokeStatic(beginAttributeMethod);
	for (int i = 1;  i < nargs;  i++)
	  args[i].compile(comp, target);
	code.emitInvokeInterface(endAttributeMethod);
      }
    else if (target instanceof IgnoreTarget)
      ApplyExp.compile(exp, comp, target);
    else
      ConsumerTarget.compileUsingConsumer(exp, comp, target);
  }

  static final ClassType typeMakeAttribute
    = ClassType.make("gnu.xquery.util.MakeAttribute");
  static final Method beginAttributeMethod
    = typeMakeAttribute.getDeclaredMethod("beginAttribute", 2);
  static final Method endAttributeMethod
    = Compilation.typeConsumer.getDeclaredMethod("endAttribute", 0);

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }
}
