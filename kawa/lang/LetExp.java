package kawa.lang;
import codegen.*;

/**
 * Class used to implement "let" syntax (and variants) for Scheme.
 * @author	Per Bothner
 */

public class LetExp extends ScopeExp
{
  Expression[] inits;
  public Expression body;

  public LetExp (Expression[] i) { inits = i; }
  //  public LetExp (Declaration[] d, Expression[] i) { decls = d;  inits = i; }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Object[] values = shared ? env.values : new Object[frameSize];
    int i = 0;
    for (Variable var = firstVar ();  var != null; var = var.nextVar ())
      {
	if (var != heapFrame)
	  values[var.offset] = inits[i].eval (env);
	i++;
      }
    return body.eval (shared ? env : new Environment (values, this, env));
  }

  /* Recursive helper routine, to store the values on the stack
   * into the variables in vars, in reverse order. */
  private final void store_rest (Compilation comp, Variable vars)
  {
    if (vars != null)
      {
	store_rest (comp, vars.nextVar ());
	SetExp.compile_store ((Declaration) vars, comp);
      }
  }

  public void compile (Compilation comp, boolean ignore_result)
  {
    /* Compile all they initializations, leaving the results
       on the stack (in reverse order).  */
    for (int i = 0; i < inits.length; i++)
      inits[i].compile (comp, false);

    comp.method.enterScope (scope);

    /* Assign the initial values to the proper variables, in reverse order. */
    store_rest (comp, firstVar ());

    body.compile (comp, ignore_result);
    comp.method.pop_scope ();
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%let (");
    Variable var = firstVar ();
    int i = 0;
    
    for (; var != null; i++, var = var.nextVar ())
      {
	if (i > 0)
	  ps.print(" ");
	ps.print("(");
	ps.print(((Declaration) var).string_name());
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
