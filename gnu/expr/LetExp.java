package gnu.expr;
import gnu.bytecode.*;

/**
 * Class used to implement "let" syntax (and variants) for Scheme.
 * @author	Per Bothner
 */

public class LetExp extends ScopeExp
{
  public Expression[] inits;
  public Expression body;

  public LetExp (Expression[] i) { inits = i; }

  Method makeBindingMethod = null;

  /* Recursive helper routine, to store the values on the stack
   * into the variables in vars, in reverse order. */
  static void store_rest (Compilation comp, Variable vars)
  {
    if (vars != null)
      {
	Declaration decl = (Declaration) vars;
	store_rest (comp, vars.nextVar ());
	if (! decl.ignorable())
	  decl.initBinding(comp);
      }
  }

  public void compile (Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();

    if (comp.usingCPStyle())
      { 
	for (Variable var = firstVar (); var != null; var = var.nextVar ())
	  {
	    ((Declaration) var).assignField(comp);
	  }
     }

    /* Compile all the initializations, leaving the results
       on the stack (in reverse order).  */
    Variable var = firstVar();
    for (int i = 0; i < inits.length; i++, var = var.nextVar())
      {
	inits[i].compile (comp,
			  ((Declaration) var).ignorable() ? Target.Ignore
			  : Target.pushObject);
      }

    code.enterScope (scope);

    /* Assign the initial values to the proper variables, in reverse order. */
    store_rest (comp, firstVar ());

    body.compileWithPosition(comp, target);
    code.popScope ();
  }

  Object walk (ExpWalker walker) { return walker.walkLetExp(this); }

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
