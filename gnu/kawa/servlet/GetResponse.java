// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

/** A 0-argument function that returns the current ServletResponse. */

public class GetResponse extends CpsProcedure implements Inlineable
{
  public static final GetResponse getResponse = new GetResponse();

  public static javax.servlet.http.HttpServletResponse getResponse(CallContext ctx)
  {
    return ((ServletCallContext) ctx).response;
  }

  public int numArgs() { return 0; }

  public void apply (CallContext ctx)
  {
    ctx.lastArg();
    ctx.consumer.writeObject(((ServletCallContext) ctx).response);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 0 || ! comp.curLambda.isHandlingTailCalls())
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }

    CodeAttr code = comp.getCode();
    comp.loadCallContext();
    code.emitInvokeStatic(typeGetResponse.getDeclaredMethod("getResponse", 1));
    target.compileFromStack(comp, typeHttpServletResponse);
  }

  public Type getReturnType (Expression[] args)
  {
    return typeHttpServletResponse;
  }

  public static final ClassType typeGetResponse
    = ClassType.make("gnu.kawa.servlet.GetResponse");

  public static final ClassType typeHttpServletResponse
  = ClassType.make("javax.servlet.http.HttpServletResponse");
}
