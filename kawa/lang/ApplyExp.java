package kawa.lang;

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
	Object randsarg = kawa.lang.Interpreter.nullObject;
	for (int i = args.length; --i >= 0; )
	  randsarg = new pair (args[i].eval (env), randsarg);
	return env.interp.apply(rator,randsarg, env.frames);
      }

    int n = args.length;
    Object[] vals = new Object[n];
    for (int i = 0; i < n; i++)
      vals[i] = args[i].eval (env);
    return ((kawa.lang.Procedure)rator).applyN (vals);
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%apply ");
    kawa.lang.print.print (func, ps);
    for (int i = 0; i < args.length; ++i)
      {
	ps.print(" ");
	args[i].print (ps);
      }
    ps.print(")");
  }

}

