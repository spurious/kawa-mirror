package kawa.lang;
import kawa.lang.*;

/**
 * Class used to implement "let" syntax (and variants) for Scheme.
 * @author	Per Bothner
 */

public class LetExp extends ScopeExp
{
  Expression[] inits;
  public Expression body;

  public LetExp (Declaration[] d, Expression[] i) { decls = d;  inits = i; }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    int n = decls.length;
    Object[] values = new Object[n];
    for (int i = 0; i < n; i++)
      values[i] = inits[i].eval (env);
    Environment new_env = new Environment (values, this, env);
    return body.eval (new_env);
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%let (");
    int n = decls.length;
    for (int i = 0; i < n; i++)
      {
	if (i > 0)
	  ps.print(" ");
	ps.print("(");
	ps.print(decls[i].string_name());
	if (inits[i] != null)
	  {
	    ps.print(" ");
	    inits[i].print (ps);
	  }
	ps.print(")");
      }
    ps.print(") ");
    body.print (ps);
    ps.print(")");
  }
}
