// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.reflect;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.mapping.*;

/** Implement 'typeswitch' (as in XQuery) or 'typecase'.
 * Usage: (typeswitch SELECTOR CASE-LAMBDA ... DEFAULT-LAMBDA)
 * Each CASE-LAMBDA is a 1-argument MethodProc, while DEFAULT-LAMBDA
 * is a 0-argument Procedure.  Calls the first CASE-LAMBDA such that
 * SELECTOR is a valid argument; if there is none, calls DEFAULT-LAMBDA.
 * In the current implementation, all of CASE-LAMBDA and DEFAULT-LAMBDA
 * must be LambdaExps, and the call must be inlined.
 */

public class TypeSwitch extends MethodProc implements CanInline, Inlineable
{
  public static final TypeSwitch typeSwitch = new TypeSwitch("typeswitch");

  public TypeSwitch(String name)
  {
    setName(name);
  }

  public int numArgs() { return 0xfffff002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    Object selector = args[0];
    int n = args.length-1;
    for (int i = 1;  i < n;  i++)
      {
	MethodProc caseProc = (MethodProc) args[i];
        int m = caseProc.match1(selector, ctx);
	if (m >= 0)
	  return;
      }
    Procedure defaultProc = (Procedure) args[n];
    defaultProc.check1(selector, ctx);
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    for (int i = 1;  i < args.length;  i++)
      {
	if (args[i] instanceof LambdaExp)
	  {
	    LambdaExp lexp = (LambdaExp) args[i];
	    lexp.setInlineOnly(true);
	    lexp.returnContinuation = exp;
	  }
      }
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();

    CodeAttr code = comp.getCode();
    code.pushScope();
    Variable selector = code.addLocal(Type.pointer_type);
    args[0].compile(comp, Target.pushObject);
    code.emitStore(selector);

    for (int i = 1;  i < args.length - 1;  i++)
      {
	if (i > 1)
	  code.emitElse();

	if (args[i] instanceof LambdaExp)
	  {
	    LambdaExp lambda = (LambdaExp) args[i];
	    Declaration param = lambda.firstDecl();
	    Type type = param.getType();
	    param.allocateVariable(code);

	    if (type instanceof TypeValue)
	      ((TypeValue) type).emitTestIf(selector, param, comp);
	    else
	      {
		code.emitLoad(selector);
		type.emitIsInstance(code);
		code.emitIfIntNotZero();

		code.emitLoad(selector);
		param.compileStore(comp);
	      }
	    lambda.allocChildClasses(comp);
	    lambda.body.compileWithPosition(comp, target);
	  }
	else
	  {
	    throw new Error("not implemented: typeswitch arg not LambdaExp");
	  }
      }
    int i = args.length - 2;
    if (i > 0)
      code.emitElse();
    LambdaExp lambda = (LambdaExp) args[args.length - 1];
    lambda.allocChildClasses(comp);
    lambda.body.compileWithPosition(comp, target); // FIXME target
    while (--i >= 0)
      code.emitFi();
    
    code.popScope();
  }


  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}
