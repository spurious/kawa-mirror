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

  /** Containing LambdaExp. */
  LambdaExp context;

  /** The next ApplyExp in ((ReferenceExp)func).binding.firstCall list. */
  public ApplyExp nextCall;

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

  public static void compileToArray(Expression[] args, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    code.emitPushInt(args.length);
    code.emitNewArray(Type.pointer_type);
    for (int i = 0; i < args.length; ++i)
      {
	code.emitDup(comp.objArrayType);
	code.emitPushInt(i);
	args[i].compile (comp, Target.pushObject);
	code.emitArrayStore(Type.pointer_type);
      }
  }

  public static void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Method method;
    gnu.bytecode.CodeAttr code = comp.getCode();
    LambdaExp func_lambda = null;
    String func_name = null;

    if (exp.func instanceof LambdaExp)
      {
	func_lambda = (LambdaExp) exp.func;
	func_name = func_lambda.getName();
	if (func_name == null)
	  func_name = "<lambda>";
      }
    else if (exp.func instanceof ReferenceExp) 
      { 
        Declaration func_decl = ((ReferenceExp)exp.func).binding; 
        if (func_decl != null && func_decl.value != null 
            && func_decl.value instanceof LambdaExp) 
	  {
	    func_lambda = (LambdaExp) func_decl.value;
	    func_name = func_decl.string_name ();
	  }
      }
    if (func_lambda != null)
      {
	// These error message should really be done earlier,
	// but we do not have the right information until
	// the rewrite pass is finished.
	if (exp.args.length < func_lambda.min_args)
	  {
	    System.err.print ("warning:  too few args for ");
	    System.err.println(func_name);
	    func_lambda = null;
	  }
	else if (func_lambda.max_args >= 0
		 && exp.args.length > func_lambda.max_args)
	  {
	    System.err.print ("warning:  too many args for ");
	    System.err.println(func_name);
	    func_lambda = null;
	  }
	else if (! func_lambda.getCanRead() && ! func_lambda.getInlineOnly())
	  {
	    method = func_lambda.primMethod;
	    boolean is_static = method.getStaticFlag();
	    Expression[] args = exp.getArgs();
	    Type[] argTypes = method.getParameterTypes();
	    // ?? Procedure.checkArgCount(this, args.length);
	    if (! is_static)
	      func_lambda.outerLambda().loadHeapFrame(comp);
	    if (func_lambda.max_args != func_lambda.min_args)
	      {
		compileToArray (exp.args, comp);
	      }
	    else
	      {
		for (int i = 0; i < args.length; ++i)
		  {
		    if (argTypes[i] == null)
		      {
			throw new Error("bad argtypes["+i+"] len:"+argTypes.length
+" func:"+func_lambda+" meth:"+method);
		      }
		    args[i].compile(comp, argTypes[i]);
		  }
	      }
	    if (is_static)
	      code.emitInvokeStatic(method);
	    else
	      code.emitInvokeVirtual(method);
	    target.compileFromStack(comp, method.getReturnType());
	    return;
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
	func_lambda.allocParameters(comp, null);
	popParams (code, func_lambda);
	func_lambda.enterFunction(comp, null);
	func_lambda.body.compileWithPosition(comp, Target.returnObject);
	code.popScope();
	comp.curLambda = saveLambda;
	target.compileFromStack(comp, Type.pointer_type);
	return;
      }

    if (!tail_recurse)
      exp.func.compile (comp, new StackTarget(comp.scmProcedureType));

    if (args_length <= 4
	|| (tail_recurse
	    && func_lambda.min_args == func_lambda.max_args))
      {
	for (int i = 0; i < args_length; ++i)
	  exp.args[i].compile (comp, Target.pushObject);
	method = comp.applymethods[args_length];
      }
    else
      {
	compileToArray(exp.args, comp);
	method = comp.applyNmethod;
      }
    if (tail_recurse)
      {
	popParams(code, func_lambda);
	code.emitTailCall(false, func_lambda.scope);
	return;
      }
    code.emitInvokeVirtual(method);
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

  private static void popParams (CodeAttr code, LambdaExp lexp)
  {
    Variable params = lexp.firstVar();
    if (params != null && params.getName() == "this")
      params = params.nextVar();
    if (params != null && params.getName() == "argsArray")
      params = params.nextVar();
    popParams (code, params, lexp.min_args);
  }

  // Recursive helper function.
  private static void popParams (CodeAttr code, Variable vars, int count)
  {
    if (count > 0)
      {
	if (! vars.isSimple())
	  vars = vars.nextVar();
	popParams (code, vars.nextVar (), count - 1);
	code.emitStore(vars);
      }
  }

  public final gnu.bytecode.Type getType()
  {
    if (func instanceof QuoteExp)
      {
	Object proc = ((QuoteExp) func).getValue();
	if (proc instanceof PrimProcedure)
	  return ((PrimProcedure) proc).getReturnType();
	if (proc instanceof kawa.lang.SetFieldProc)
	  return Type.void_type;
      }
    return super.getType();
  }

}
