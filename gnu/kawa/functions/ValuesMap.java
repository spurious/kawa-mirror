// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

/** Map a unary function over a value sequence, yielding a new sequence.
 * Used to implement XQuery's 'for' form.
 */

public class ValuesMap extends CpsProcedure implements CanInline, Inlineable
{
  public static final ValuesMap valuesMap = new ValuesMap();

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Procedure proc = (Procedure) ctx.getNextArg();
    Consumer out = ctx.consumer;
    Object val = ctx.getNextArg();
    Procedure.checkArgCount(proc, 1);
    if (val instanceof Values)
      {
	int ipos = 0;
	Values values = (Values) val;
	while ((ipos = values.nextPos(ipos)) != 0)
	  {
	    Object v = values.getPosPrevious(ipos);
	    ctx.setArgs(v);
	    ctx.proc = proc;
	    ctx.runUntilDone();
	  }
      }
    else
      {
	ctx.setArgs(val);
	ctx.proc = proc;
	ctx.runUntilDone();
      }
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 2
	&& args[0] instanceof LambdaExp)
      {
	LambdaExp lexp = (LambdaExp) args[0];
	lexp.setInlineOnly(true);
	lexp.returnContinuation = exp;
      }
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    if (! (target instanceof IgnoreTarget
	   || target instanceof ConsumerTarget
	   || target instanceof SeriesTarget))
      {
	ConsumerTarget.compileUsingConsumer(exp, comp, target);
	return;
      }
    Expression vals = args[1];
    LambdaExp lambda;
    if (args[0] instanceof LambdaExp)
      lambda = (LambdaExp) args[0];
    else
      {
	// FIXME in InlineCalls phase could wrap expr in LambdaExp
	ApplyExp.compile(exp, comp, target);
	return;
      }
    Declaration param = lambda.firstDecl();
    if (param == null || param.nextDecl() != null)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    CodeAttr code = comp.getCode();
    SeriesTarget starget = new SeriesTarget();
    starget.scope = code.pushScope();
    starget.function = new Label(code);
    starget.done = new Label(code);
    // If the param Declaration is captured, then it gets messy initializing
    // it.  So just cheat and create a helper varaible.
    if (param.isSimple())
      param.allocateVariable(code);
    else
      param = new Declaration(code.addLocal(param.getType(), param.getName()));
    starget.param = param;
    Type retAddrType = Type.pointer_type;
    Variable retAddr = code.addLocal(retAddrType);
    vals.compileWithPosition(comp, starget);

    if (code.reachableHere())
      code.emitGoto(starget.done);
    starget.function.define(code);
    code.pushType(retAddrType);
    code.emitStore(retAddr);
    args = new Expression[] { new ReferenceExp(param) };
    new ApplyExp(lambda, args).compile(comp, target);
    code.emitRet(retAddr);
    code.popScope();
    starget.done.define(code);
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}
