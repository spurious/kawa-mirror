package kawa.lang;
import codegen.*;

/** This class is used to represent "combination" or "application".
 * A function and arguments are evaluated, and then the function applied.
 * @author	Per Bothner
 */

public class ApplyExp extends Expression
{
  Expression func;
  Expression[] args;

  public ApplyExp (Expression f, Expression[] a) { func = f; args = a; }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Object rator = func.eval(env);

    if (! (rator instanceof kawa.lang.Procedure))
      {
	System.err.print("[Obsolete-style ApplyExp.eval]\n");
	Object randsarg = List.Empty;
	for (int i = args.length; --i >= 0; )
	  randsarg = new Pair (args[i].eval (env), randsarg);
	return env.interp.apply(rator,randsarg, env.frames);
      }

    int n = args.length;
    Object[] vals = new Object[n];
    for (int i = 0; i < n; i++)
      vals[i] = args[i].eval (env);
    return ((kawa.lang.Procedure)rator).applyN (vals);
  }

  public void compile (Compilation comp, boolean ignore_result)
  {
    Method applymethod;
    func.compile (comp, false);
    comp.method.compile_checkcast (comp.scmProcedureType);
    if (args.length <= 4)
      {
	for (int i = 0; i < args.length; ++i)
	  args[i].compile (comp, false);
	applymethod = comp.applymethods[args.length];
      }
    else
      {
	comp.method.compile_push_int (args.length);
	comp.method.compile_new_array (comp.scmObjectType);
	for (int i = 0; i < args.length; ++i)
	  {
	    comp.method.compile_dup (comp.scmObjectType);
	    comp.method.compile_push_int (i);
	    args[i].compile (comp, false);
	    comp.method.compile_array_store (comp.scmObjectType);
	  }
	applymethod = comp.applyNmethod;
      }
    comp.method.compile_invoke_virtual (applymethod);
    if (ignore_result)
      comp.method.compile_pop (1);
  }

  public void print (java.io.PrintStream ps)
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
