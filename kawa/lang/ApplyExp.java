package kawa.lang;
import gnu.bytecode.*;

/** This class is used to represent "combination" or "application".
 * A function and arguments are evaluated, and then the function applied.
 * @author	Per Bothner
 */

public class ApplyExp extends Expression
{
  Expression func;
  Expression[] args;

  public Expression[] getArgs() { return args; }

  public ApplyExp (Expression f, Expression[] a) { func = f; args = a; }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Object rator = func.eval(env);
    int n = args.length;
    Object[] vals = new Object[n];
    for (int i = 0; i < n; i++)
      vals[i] = args[i].eval (env);
    return ((Procedure)rator).applyN (vals);
  }

  public void compile (Compilation comp, int flags)
  {
    if (func instanceof QuoteExp)
      {
	Object proc = ((QuoteExp) func).value;
	if (proc instanceof Inlineable)
	  {
	    ((Inlineable) proc).compile(this, comp, flags);
	    return;
	  }
      }

    Method applymethod;
    gnu.bytecode.CodeAttr code = comp.getCode();
    LambdaExp func_lambda = null;

    if (func instanceof ReferenceExp)
      {
	Declaration func_decl = ((ReferenceExp)func).binding;
	if (func_decl != null && func_decl.value != null
	    && func_decl.value instanceof LambdaExp)
	  {
	    // These error message should really be done earlier,
	    // but we do not have the right information until
	    // the rewrite pass is finished.
	    func_lambda = (LambdaExp) func_decl.value;
	    if (args.length < func_lambda.min_args)
	      {
		System.err.print ("warning:  too few args for ");
		System.err.println (func_decl.string_name ());
		func_lambda = null;
	      }
	    else if (func_lambda.max_args >= 0
		     && args.length > func_lambda.max_args)
	      {
		System.err.print ("warning:  too many args for ");
		System.err.println (func_decl.string_name ());
		func_lambda = null;
	      }
	  }
      }

    // Check for tail-recursion.
    boolean tail_recurse
      = (flags & LAST) != 0
      && func_lambda != null && func_lambda == comp.curLambda;

    if (!tail_recurse)
      {
	func.compile (comp, 0);
	comp.method.compile_checkcast (comp.scmProcedureType);
      }

    if (tail_recurse
	&& func_lambda.min_args == func_lambda.max_args)
      {
	for (int i = 0; i < args.length; ++i)
	  args[i].compile (comp, 0);
	for (int i = args.length;  --i >= 0; )
	  SetExp.compile_store (func_lambda.getArg (i), comp);
	code.emitGoto(func_lambda.start_label);
	return;
      }

    if (args.length <= 4)
      {
	for (int i = 0; i < args.length; ++i)
	  args[i].compile (comp, 0);
	applymethod = comp.applymethods[args.length];
      }
    else
      {
	code.emitPushInt(args.length);
	code.emitNewArray(comp.scmObjectType);
	for (int i = 0; i < args.length; ++i)
	  {
	    code.emitDup(comp.objArrayType);
	    code.emitPushInt(i);
	    args[i].compile (comp, 0);
	    comp.method.compile_array_store (comp.scmObjectType);
	  }
	applymethod = comp.applyNmethod;
      }
    if (tail_recurse)
      {
	comp.method.compile_tailcall (true);
	return;
      }
    comp.method.compile_invoke_virtual (applymethod);
    if ((flags & IGNORED) != 0)
      code.emitPop(1);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%apply ");
    func.print (ps);
    for (int i = 0; i < args.length; ++i)
      {
	ps.print(" ");
	args[i].print (ps);
      }
    ps.print(")");
  }

}
