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
    Object[] values = shared ? env.values : new Object[space_needed];
    for (int i = 0; i < num_decls; i++)
      values[decls[i].index] = inits[i].eval (env);
    return body.eval (shared ? env : new Environment (values, this, env));
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%let (");
    for (int i = 0; i < num_decls; i++)
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
