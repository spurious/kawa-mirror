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

  public ApplyExp (Procedure p, Expression[] a) { func = new QuoteExp(p); args = a; }

  public Object eval (Environment env)
  {
    Procedure proc = (Procedure) func.eval(env);
    int n = args.length;
    Object[] vals = new Object[n];
    for (int i = 0; i < n; i++)
      vals[i] = args[i].eval (env);
    return proc.applyN (vals);
  }

  public void compile (Compilation comp, Target target)
  {
    if (func instanceof QuoteExp)
      {
	Object proc = ((QuoteExp) func).getValue();
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
    if (args.length == 0)
      {
	code.emitGetStatic(Compilation.noArgsField);
	return;
      }
    LambdaExp caller = comp.curLambda;
    if (args.length == caller.min_args
	&& args.length == caller.max_args
	&& caller.isHandlingTailCalls())
      {
	// Re-use caller's argsArray.
	// code.emitLoad(caller.declareArgsArray());  FIXME
	code.emitLoad(comp.callStackContext);
	code.emitGetField(Compilation.argsCallStackField);
      }
    else
      {
	code.emitPushInt(args.length);
	code.emitNewArray(Type.pointer_type);
      }
    for (int i = 0; i < args.length; ++i)
      {
	Expression arg = args[i];
	if (comp.usingCPStyle
	    && ! (arg instanceof QuoteExp) && ! (arg instanceof ReferenceExp))
	  {
	    // If the argument involves a CPStyle function call, we will
	    // have to save and restore anything on the JVM stack into
	    // fields in the CallFrame.  This is expensive, so defer
	    // pushing the duplicated argument array and the index
	    // until *after* we've calculated the argument.  The downside
	    // is that we have to do some extra stack operations.
	    // However, these are cheap (and get compiled away when
	    // compiling to native code).
	    arg.compile (comp, Target.pushObject);
	    code.emitSwap();
	    code.emitDup(1, 1);
	    code.emitSwap();
	    code.emitPushInt(i);
	    code.emitSwap();
	  }
	else
	  {
	    code.emitDup(comp.objArrayType);
	    code.emitPushInt(i);
	    arg.compile (comp, Target.pushObject);
	  }
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
        if (func_decl != null)
	  {
	    Expression value = func_decl.getValue();
	    if (value != null && value instanceof LambdaExp) 
	      {
		func_lambda = (LambdaExp) value;
		func_name = func_decl.getName();
	      }
	  }
      }
    else if (exp.func instanceof QuoteExp)
      {
        Object proc = ((QuoteExp) exp.func).getValue();
        // If it wasn't inlineable, we already checked for this in Translator.
        if (proc instanceof Inlineable)
          {
            PrimProcedure pproc
              = PrimProcedure.getMethodFor((Procedure) proc, exp.args);
            if (pproc != null)
              {
                exp = new ApplyExp(pproc, exp.args);
                ((Inlineable) pproc).compile(exp, comp, target);
                return;
              }
          }
      }
    if (func_lambda != null)
      {
	// These error message should really be done earlier,
	// but we do not have the right information until
	// the rewrite pass is finished.
	if (exp.args.length < func_lambda.min_args)
	  {
            comp.error('w', "too few args for " + func_name);
	    func_lambda = null;
	  }
	else if (func_lambda.max_args >= 0
		 && exp.args.length > func_lambda.max_args)
	  {
            comp.error('w', "too many args for " + func_name);
	    func_lambda = null;
	  }
	else if (! func_lambda.isHandlingTailCalls()
		 && (method = func_lambda.getMethod(exp.args.length)) != null)
	  {
	    boolean is_static = method.getStaticFlag();
	    Expression[] args = exp.getArgs();
	    int extraArg = 0;
	    Type[] argTypes = method.getParameterTypes();
	    // ?? Procedure.checkArgCount(this, args.length); // FIXME
	    LambdaExp parent = func_lambda.outerLambda();
	    if (! is_static || func_lambda.declareClosureEnv() != null)
	      {
		if (is_static)
		  extraArg = 1;
		if (comp.curLambda == func_lambda)
		  code.emitLoad(func_lambda.closureEnv);  // Recursive call.
		else if (parent.heapFrame != null || parent.closureEnv == null)
		  parent.loadHeapFrame(comp);
		else
		  code.emitLoad(parent.closureEnv);
	      }

	    boolean varArgs = func_lambda.restArgType() != null;
	    PrimProcedure.compileArgs(args,
				      extraArg > 0 ? Type.void_type : null,
				      argTypes, varArgs,
				      func_name, func_lambda, comp);
	    code.emitInvoke(method);
	    target.compileFromStack(comp, method.getReturnType());
	    return;
	  }
      }

    if (comp.usingCPStyle())
      {
	  {
	    Label l = new Label(code);
	    gnu.bytecode.SwitchState fswitch = comp.fswitch;
	    int pc = fswitch.getMaxValue() + 1;
	    fswitch.addCase(pc, l, code);
	    code.emitLoad(comp.callStackContext);

	    // Emit: context->pc = pc.
	    code.emitLoad(comp.callStackContext);
	    code.emitPushInt(pc);
	    code.emitPutField(Compilation.pcCallStackField);

	    code.emitInvokeVirtual(comp.applyCpsMethod);

	    // emit[save java stack, if needed]
	    Type[] stackTypes = code.saveStackTypeState(false);
	    java.util.Stack stackFields = new java.util.Stack(); 
	    if (stackTypes != null)
	      {
		for (int i = stackTypes.length;  --i >= 0; )
		  {
		    Field fld = comp.allocLocalField (stackTypes[i], null);
		    code.emitPushThis();
		    code.emitSwap();
		    code.emitPutField(fld);
		    stackFields.push(fld);
		  }
	      }

	    code.emitReturn();
	    l.define(code);

	    // emit[restore java stack, if needed]
	    if (stackTypes != null)
	      {
		for (int i = stackTypes.length;  --i >= 0; )
		  {
		    Field fld = (Field) stackFields.pop();
		    code.emitPushThis();
		    code.emitGetField(fld);
		    comp.freeLocalField(fld);
		  }
	      }

	    // Load result from stack.value to target.
	    code.emitLoad(comp.callStackContext);
	    code.emitGetField(comp.valueCallStackField);
	    target.compileFromStack(comp, Type.pointer_type);
	  }
	return;
      }

    int args_length = exp.args.length;

    // Check for tail-recursion.
    boolean tail_recurse
      = exp.tailCall
      && func_lambda != null && func_lambda == comp.curLambda;

    if (func_lambda != null && func_lambda.getInlineOnly() && !tail_recurse
	&& func_lambda.min_args == args_length)
      {
	for (int i = 0; i < args_length; ++i)
	  exp.args[i].compile (comp, Target.pushObject);
	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = func_lambda;
	func_lambda.allocChildClasses(comp);
	func_lambda.allocParameters(comp);
	popParams (code, func_lambda, false);
	func_lambda.enterFunction(comp);
	func_lambda.body.compileWithPosition(comp, target);
	code.popScope();
	// comp.method.popScope();
	func_lambda.compileChildMethods(comp);
	comp.curLambda = saveLambda;
	return;
      }

    if (comp.curLambda.isHandlingTailCalls() && exp.isTailCall()
	&& ! comp.curLambda.getInlineOnly())
      {
	code.emitLoad(comp.callStackContext);
	code.emitDup(comp.callStackContext.getType());
	exp.func.compile(comp, new StackTarget(comp.typeProcedure));
	code.emitPutField(comp.procCallStackField);
	code.emitDup(comp.callStackContext.getType());
	//  evaluate args to frame-locals vars;  // may recurse! 
	compileToArray (exp.args, comp);
	code.emitPutField(comp.argsCallStackField);
	code.emitReturn();
	return;
      }

    if (!tail_recurse)
      exp.func.compile (comp, new StackTarget(comp.typeProcedure));

    boolean toArray
      = (tail_recurse ? func_lambda.min_args != func_lambda.max_args
         : args_length > 4);
    if (! toArray)
      {
	for (int i = 0; i < args_length; ++i)
	  exp.args[i].compile (comp, Target.pushObject);
        method = tail_recurse ? null : comp.applymethods[args_length];
      }
    else
      {
	compileToArray(exp.args, comp);
	method = comp.applyNmethod;
      }
    if (tail_recurse)
      {
	popParams(code, func_lambda, toArray);
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

  private static void popParams (CodeAttr code, LambdaExp lexp,
                                 boolean toArray)
  {
    Variable params = lexp.scope.firstVar();
    if (params != null && params.getName() == "this")
      params = params.nextVar();
    if (params != null && params.getName() == "argsArray")
      {
        if (toArray)
          {
            popParams (code, params, 1);
            return;
          }
        params = params.nextVar();
      }
    popParams (code, params, lexp.min_args);
  }

  // Recursive helper function.
  private static void popParams (CodeAttr code, Variable vars, int count)
  {
    if (count > 0)
      {
	popParams (code, vars.nextVar(), count - 1);
	code.emitStore(vars);
      }
  }

  public final gnu.bytecode.Type getType()
  {
    if (func instanceof QuoteExp)
      {
	Object proc = ((QuoteExp) func).getValue();
	if (proc instanceof Inlineable)
	  return ((Inlineable) proc).getReturnType(args);
      }
    return super.getType();
  }

}
