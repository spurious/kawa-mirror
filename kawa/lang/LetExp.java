package kawa.lang;
import gnu.bytecode.*;

/**
 * Class used to implement "let" syntax (and variants) for Scheme.
 * @author	Per Bothner
 */

public class LetExp extends ScopeExp
{
  Expression[] inits;
  public Expression body;

  public LetExp (Expression[] i) { inits = i; }

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

  public void compile (Compilation comp, int flags)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    /* Compile all the initializations, leaving the results
       on the stack (in reverse order).  */
    for (int i = 0; i < inits.length; i++)
      inits[i].compile (comp, 0);

    code.enterScope (scope);

    /* Assign the initial values to the proper variables, in reverse order. */
    store_rest (comp, firstVar ());

    body.compile_with_linenumber (comp, flags);
    code.popScope ();
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%let (");
    Variable var = firstVar ();
    int i = 0;
    
    for (; var != null; var = var.nextVar ())
      {
	if (i > 0)
	  ps.print(" ");
	ps.print("(");
	ps.print(((Declaration) var).string_name());
	ps.print(" ");
	if (var.isArtificial ())
	  ps.print ("<artificial>");
	else
	  {
	    if (inits[i] == null)
	      ps.print ("<null>");
	    else
	      inits[i].print (ps);
	    i++;
	  }
	ps.print(")");
      }
    ps.print(") ");
    body.print (ps);
    ps.print(")");
  }
}
