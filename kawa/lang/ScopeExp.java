package kawa.lang;
import codegen.*;

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
  ScopeExp outer;

  /** Number of frame slots needed by the scope, and shared inner scopes. */
  // FIXME - this is obsolete, except for eval'd code.
  int space_needed;

  /** True iff this scope should be embedded in the outer scope's frame. */
  boolean shared;

  public void assign_space ()
  {
    LambdaExp lambda = currentLambda ();
    /* If space_need > 0, we have already allocated space for nested scopes.
     * Otherwise, make sure incoming arguments are properly allocated. */
    if (lambda != null && space_needed == 0)
      space_needed = lambda.incomingArgs ();
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ()) 
      {
	Declaration decl = (Declaration) var;
	if (decl.offset < 0)
	  {
	    if (! decl.isSimple () && lambda != null)
	      {
	      /* A variable captured by an inner Lambda is allocated
		 in the heap frame. */
		if (lambda.heapFrame == null)
		  lambda.heapFrame = add_decl (Symbol.make ("heapFrame"),
					       Compilation.objArrayType);
		decl.baseVariable = lambda.heapFrame;
		decl.offset = lambda.heapSize++;
	      }
	  }
      }
    if (shared && space_needed > outer.space_needed)
      outer.space_needed = space_needed;
  }

  public final void push_decls (Interpreter interp)
  {
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
      {
	if (! var.isArtificial ())
	  ((Declaration)var).push (interp);
      }
  }

  public final void pop_decls (Interpreter interp)
  {
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
      {
	if (! var.isArtificial ())
	  ((Declaration)var).pop (interp);
      }
  }

  public LambdaExp currentLambda ()
  {
    ScopeExp exp = this;
    for (;; exp = exp.outer)
      {
	if (exp == null)
	  return null;
	if (exp instanceof LambdaExp)
	  return (LambdaExp) exp;
      }
  }

  public void push (Interpreter interp)
  {
    outer = interp.current_scope;
    if (outer != null && ! (this instanceof LambdaExp))
      shared = true;
    interp.current_scope = this;
    push_decls (interp);
  }

  public void pop (Interpreter interp)
  {
    assign_space ();
    pop_decls (interp);
    interp.current_scope = outer;
  }

  /**
   * Find a Declaration by name.
   * @param sym the name of the Declaration sought
   * @return the matching Declaration, if found;  otherwise null
   */
  Declaration lookup (Symbol sym)
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
   * @param name name to give to the new Declaration.
   */
  public final Declaration add_decl (Symbol name)
  {
    Declaration decl = new Declaration (name);
    add_decl (decl);
    return decl;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name to give to the new Declaration.
   * @param type type of the new Declaration.
   */
  public final Declaration add_decl (Symbol name, Type type)
  {
    Declaration decl = new Declaration (name);
    add_decl (decl);
    decl.type = type;
    return decl;
  }

  /**
   * Add a Declaration to the current Scope.
   */
  public final void add_decl (Declaration decl)
  {
    scope.add_var (decl);
    decl.context = this;
  }
}
