package kawa.lang;

/**
 * Abstract class for expressions that add local variable bindings.
 * @author	Per Bothner
 */

public abstract class ScopeExp extends Expression
{
  /** The number of Declarations declared by this scope. */
  int num_decls;

  /** The set of declarations (or bindings) provided by this scope.
   * The decls[num_decls..decls.length are null (space to grow). */
  Declaration[] decls;

  /** The statically enclosing binding contour. */
  ScopeExp outer;

  /** Number os frame slots needed by the scope, and shared inner scopes. */
  int space_needed;

  /** True iff this scope should be embedded in the outer scope's frame. */
  boolean shared;

  // Space in a shared frame must be allocated in a depth-first order.
  // I.e. we assign space on scope *exit*.
  public void assign_space ()
  {
    for (int i = 0;  i < num_decls; i++)
      {
	Declaration decl = decls[i];
	if (decl.index < 0)
	  decl.index = space_needed++;
      }
    if (shared && space_needed > outer.space_needed)
      outer.space_needed = space_needed;
  }

  public final void push_decls (Interpreter interp)
  {
    for (int i = 0;  i < num_decls; i++)
      decls[i].push (interp);
  }

  public final void pop_decls (Interpreter interp)
  {
    for (int i = num_decls; --i >= 0; )
      decls[i].pop (interp);
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
    for (int i = num_decls; --i >= 0; )
      {
	Declaration decl = decls[i];
	if (decl.sym == sym)
	  return decl;
      }
    return null;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name to give to the new Declaration.
   */
  public final void add_decl (Symbol name)
  {
    add_decl (new Declaration (name));
  }

  /**
   * Add a Declaration to the current Scope.
   */
  public final void add_decl (Declaration decl)
  {
    if (decls == null)
      decls = new Declaration[8];
    else if (num_decls == decls.length)
      {
	Declaration[] new_decls = new Declaration[2 * decls.length];
	System.arraycopy (decls, 0, new_decls, 0, decls.length);
	decls = new_decls;
      }
    decls[num_decls] = decl;
    num_decls++;
    decl.context = this;
  }
}
