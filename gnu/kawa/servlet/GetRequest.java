// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

/** A 0-argument function that returns the current ServletRequest. */

public class GetRequest extends MethodProc implements Inlineable
{
  public static final GetRequest getRequest = new GetRequest();

  public int numArgs() { return 0; }

  public static javax.servlet.http.HttpServletRequest getRequest(CallContext ctx)
  {
    return ((ServletCallContext) ctx).request;
  }

  public void apply (CallContext ctx)
  {
    ctx.lastArg();
    ctx.consumer.writeObject(((ServletCallContext) ctx).request);
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
    code.emitInvokeStatic(typeGetRequest.getDeclaredMethod("getRequest", 1));
    target.compileFromStack(comp, typeHttpServletRequest);
  }

  public Type getReturnType (Expression[] args)
  {
    return typeHttpServletRequest;
  }

  public static final ClassType typeGetRequest
    = ClassType.make("gnu.kawa.servlet.GetRequest");

  public static final ClassType typeHttpServletRequest
  = ClassType.make("javax.servlet.http.HttpServletRequest");
}
