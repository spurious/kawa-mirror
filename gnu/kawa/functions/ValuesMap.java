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
	for (;;)
	  {
	    Object v = values.getNext(ipos, null);
	    if (v == Sequence.eofValue)
	      break;
	    ctx.setArgs(v);
	    ctx.proc = proc;
	    ctx.runUntilDone();
	    ipos = values.nextDataIndex(ipos >> 1);
	    /*
	    if (ipos < 0)
	      break;
	    */
	    ipos = ipos << 1;
	  }
      }
    else
      {
	ctx.setArgs(val);
	ctx.proc = proc;
	ctx.runUntilDone();
      }
  }

  public Expression inline (ApplyExp exp)
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
    if (! (target instanceof SeriesTarget))
      code.pushScope();
    SeriesTarget starget = new SeriesTarget();
    starget.function = new Label(code);
    starget.done = new Label(code);
    starget.value = param.allocateVariable(code);
    Type retAddrType = Type.pointer_type;
    Variable retAddr = code.addLocal(retAddrType);
    vals.compileWithPosition(comp, starget);

    if (code.reachableHere())
      code.emitGoto(starget.done);
    starget.function.define(code);
    code.pushType(retAddrType);
    code.emitStore(retAddr);
    lambda.allocChildClasses(comp);
    lambda.body.compileWithPosition(comp, target);
    code.emitRet(retAddr);
    if (! (target instanceof SeriesTarget))
      code.popScope();
    starget.done.define(code);
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}
