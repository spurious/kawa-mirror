package kawa.lang;

/**
 * This class rerpesents a sequence of Expressions.
 * The expressions are evaluated for their side-effects,
 * and the value of the last Expression is the result.
 * @author	Per Bothner
 */

public class BeginExp extends Expression
{
  Expression[] exps;

  public BeginExp (Expression[] ex) { exps = ex; }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    int n = exps.length;
    int i;
    for (i = 0; i < n - 1; i++)
      exps[i].eval (env);
    return exps[i].eval (env);
  }

  public void compile (Compilation comp, int flags)
  {
    int n = exps.length, i;
    for (i = 0; i < n - 1; i++)
      exps[i].compile_with_linenumber (comp, IGNORED);
    exps[i].compile_with_linenumber (comp, flags);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%begin");
    int n = exps.length;
    for (int i = 0; i < n; i++)
      { 
	ps.print('\n');
	exps[i].print (ps);
      }
    ps.print(")");
  }
}
