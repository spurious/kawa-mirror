package gnu.expr;
import gnu.bytecode.*;

/**
 * Abstract class for expressions that add local variable bindings.
 * @author	Per Bothner
 */

public abstract class ScopeExp extends Expression
{
  Scope scope;

  public final Variable firstVar () { return scope.firstVar (); }

  public ScopeExp () { scope = new Scope (); }

  /** The statically enclosing binding contour. */
  public ScopeExp outer;

  public LambdaExp currentLambda ()
  {
    ScopeExp exp = this;
    for (;; exp = exp.outer)
      {
	if (exp == null)
	  return null;
	if (exp instanceof LambdaExp && ! ((LambdaExp) exp).getInlineOnly())
	  return (LambdaExp) exp;
      }
  }

  /**
   * Find a Declaration by name.
   * @param sym the (interned) name of the Declaration sought
   * @return the matching Declaration, if found;  otherwise null
   */
  public Declaration lookup (String sym)
  {
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
      {
	Declaration decl = (Declaration) var;
	if (decl.sym == sym)
	  return decl;
      }
    return null;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   */
  public final Declaration addDeclaration (String name)
  {
    Declaration decl = new Declaration (name);
    addDeclaration(decl);
    return decl;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   * @param type type of the new Declaration.
   */
  public final Declaration addDeclaration (String name, Type type)
  {
    Declaration decl = new Declaration (name);
    addDeclaration(decl);
    decl.setType(type);
    return decl;
  }

  /**
   * Add a Declaration to the current Scope.
   */
  public final void addDeclaration (Declaration decl)
  {
    scope.addVariable (decl);
    decl.context = this;
  }

  public int countDecls ()
  {
    int n = 0;
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
      n++;
    return n;
  }

  Object walk (ExpWalker walker) { return walker.walkScopeExp(this); }
}
