package kawa.lang;

/**
 * The static information associated with a local variable binding.
 * @author	Per Bothner
 */

public class Declaration
{
  /** The name of the new variable. */
  symbol sym;

  /** The variable's index in an Environment frame. */
  public int index;

  /** If non-null, the Declaration that we "shadow" (hide). */
  Declaration shadowed;

  public Declaration (symbol s) { sym = s; }

  public String string_name () { return sym.toString (); }

  /**
   * Insert this into Interpreter.current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void push (Interpreter interp)
  {
    Declaration old_decl = (Declaration) interp.current_decls.get (sym);
    if (old_decl != null)
      shadowed = old_decl;
    interp.current_decls.put (sym, this);
  }

  /** Remove this from Interpreter.current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void pop (Interpreter interp)
  {
    if (shadowed == null)
      interp.current_decls.remove (sym);
    else
      interp.current_decls.put (sym, shadowed);
  }
}
