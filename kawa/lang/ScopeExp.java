package kawa.lang;

/**
 * Abstract class for expressions that add local variable bindings.
 * @author	Per Bothner
 */

public abstract class ScopeExp extends Expression
{
  /** The set of declarations (ne bindings) provided by this scope. */
  Declaration[] decls;

  /** The statically enclosing binding contour. */
  ScopeExp outer;

  public final void push_decls (Interpreter interp)
  {
    for (int i = decls.length; --i >= 0; )
      decls[i].push (interp);
  }

  public final void pop_decls (Interpreter interp)
  {
    for (int i = decls.length; --i >= 0; )
      decls[i].pop (interp);
  }

  public void push (Interpreter interp)
  {
    push_decls (interp);
    outer = interp.current_scope;
    interp.current_scope = this;
  }

  public void pop (Interpreter interp)
  {
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
    for (int i = decls.length; --i >= 0; )
      {
	Declaration decl = decls[i];
	if (decl.sym == sym)
	  return decl;
      }
    return null;
  }
				   
}
