package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/** This class is used to represent "combination" or "application".
 * A function and arguments are evaluated, and then the function applied.
 * @author	Per Bothner
 */

public class ApplyExp extends Expression
{
  Expression func;
  Expression[] args;
  boolean tailCall;

  public final Expression getFunction() { return func; }
  public final Expression[] getArgs() { return args; }
  public final boolean isTailCall() { return tailCall; }
  public final void setTailCall(boolean tailCall) { this.tailCall = tailCall; }

  public ApplyExp (Expression f, Expression[] a) { func = f; args = a; }

  public Object eval (Environment env)
  {
    Object rator = func.eval(env);
    int n = args.length;
    Object[] vals = new Object[n];
    for (int i = 0; i < n; i++)
      vals[i] = args[i].eval (env);
    return ((Procedure)rator).applyN (vals);
  }

  public void compile (Compilation comp, Target target)
  {
    if (func instanceof QuoteExp)
      {
	Object proc = ((QuoteExp) func).value;
	if (proc instanceof Inlineable)
	  {
	    ((Inlineable) proc).compile(this, comp, target);
	    return;
	  }
      }
    compile(this, comp, target);
  }

  public static void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Method applymethod;
    gnu.bytecode.CodeAttr code = comp.getCode();
    LambdaExp func_lambda = null;

    if (exp.func instanceof ReferenceExp)
      {
	Declaration func_decl = ((ReferenceExp)exp.func).binding;
	if (func_decl != null && func_decl.value != null
	    && func_decl.value instanceof LambdaExp)
	  {
	    // These error message should really be done earlier,
	    // but we do not have the right information until
	    // the rewrite pass is finished.
	    func_lambda = (LambdaExp) func_decl.value;
	    if (exp.args.length < func_lambda.min_args)
	      {
		System.err.print ("warning:  too few args for ");
		System.err.println (func_decl.string_name ());
		func_lambda = null;
	      }
	    else if (func_lambda.max_args >= 0
		     && exp.args.length > func_lambda.max_args)
	      {
		System.err.print ("warning:  too many args for ");
		System.err.println (func_decl.string_name ());
		func_lambda = null;
	      }
	  }
      }

    /* CPS:
    if (comp.usingCPSstyle())
      {
	//  evaluate args to frame-locals vars;  // may recurse! 
	if (isTailCall())
	  {
	    emit[proc.apply(args, context, THIS_frame.caller, THIS_frame.saved_pc)];
	     code.emitReturn();
	  }
	else
	  {
	    Label l = new Label(code);
	    gnu.bytecode.SwitchState fswitch = comp.fswitch;
	    int pc = fswitch.getMaxValue() + 1;
	    fswitch.addCase(pc, l, comp);
	    emit[proc.apply(args, context, THIS_frame, pc)];
	    emit[save java stack, if needed];
	    code.emitReturn();
	    l.define(code);
	    emit[restore java stack, if needed];
	  }
	return;
      }
    */

    int args_length = exp.args.length;

    // Check for tail-recursion.
    boolean tail_recurse
      = (target instanceof TailTarget)
      && func_lambda != null && func_lambda == comp.curLambda;

    if (func_lambda != null && func_lambda.getInlineOnly() && !tail_recurse
	&& func_lambda.min_args == args_length)
      {
	for (int i = 0; i < args_length; ++i)
	  exp.args[i].compile (comp, Target.pushObject);
	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = func_lambda;
	code.enterScope (func_lambda.scope);
	/* Assign the initial values to the proper variables, in reverse order. */
	store_rest (comp, func_lambda.firstVar ());
	code.beginScope(); // ???

	func_lambda.body.compileWithPosition(comp, Target.returnObject);
	code.popScope();
	comp.curLambda = saveLambda;
	target.compileFromStack(comp, Type.pointer_type);
	return;
      }

    if (!tail_recurse)
      {
	exp.func.compile (comp, Target.pushObject);
	code.emitCheckcast(comp.scmProcedureType);
      }

    if (args_length <= 4
	&& (! tail_recurse
	    || func_lambda.min_args == func_lambda.max_args))
      {
	for (int i = 0; i < args_length; ++i)
	  exp.args[i].compile (comp, Target.pushObject);
	applymethod = comp.applymethods[args_length];
      }
    else
      {
	code.emitPushInt(args_length);
	code.emitNewArray(comp.scmObjectType);
	for (int i = 0; i < args_length; ++i)
	  {
	    code.emitDup(comp.objArrayType);
	    code.emitPushInt(i);
	    exp.args[i].compile (comp, Target.pushObject);
	    code.emitArrayStore(comp.scmObjectType);
	  }
	applymethod = comp.applyNmethod;
      }
    if (tail_recurse)
      {
	if (func_lambda.argsArray == null)
	  {
	    store_rest (comp, func_lambda.firstVar ());
	  }
	else
	  {
	    // FIXME Inefficient temporary implementation.
	    code.emitLoad(func_lambda.argsArray);
	  }
	code.emitTailCall(false, func_lambda.scope);
	return;
      }
    code.emitInvokeVirtual(applymethod);
    target.compileFromStack(comp, Type.pointer_type);
  }

  Object walk (ExpWalker walker) { return walker.walkApplyExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%apply ");
    if (tailCall)
      ps.print ("[tailcall] ");
    func.print (ps);
    for (int i = 0; i < args.length; ++i)
      {
	ps.print(" ");
	args[i].print (ps);
      }
    ps.print(")");
  }

  /* Recursive helper routine, to store the values on the stack
   * into the variables in vars, in reverse order. */
  private static void store_rest (Compilation comp, Variable vars)
  {
    if (vars != null)
      {
	store_rest (comp, vars.nextVar ());
	if (! vars.isArtificial())
	  ((Declaration) vars).initBinding(comp);
      }
  }

}
