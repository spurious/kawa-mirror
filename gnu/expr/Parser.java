package gnu.expr;

/** A very abstract "parser".
 * Converts some input representation to Expression trees. */

public abstract class Parser
{
  public abstract Expression parse (Object input);

  public Interpreter getInterpreter() { return Interpreter.getInterpreter(); }

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  /** Note that we have seen a construct that must be compiled, not evaluated.
   * If we are not inside a lambda (which is always compiled), but
   * only inside the outer-most ModuleExp, note that it must be compiled. */
  public void mustCompileHere ()
  {
    LambdaExp lambda = currentLambda ();
    if (lambda instanceof ModuleExp)
      ((ModuleExp)lambda).mustCompile = true;
  }

  public ScopeExp currentScope() { return current_scope; }

  public abstract boolean popBinding();

  public abstract void pushBinding(String name, Object value);

 /**
   * Insert decl into environ.
   * (Used at rewrite time, not eval time.)
   */
  public void push (Declaration decl)
  {
    String sym = decl.getName();
    if (sym == null)
      return;
    pushBinding(sym, decl);
  }

  /** Remove this from Translator.environ.
   * (Used at rewrite time, not eval time.)
   */
  void pop (Declaration decl)
  {
    String sym = decl.getName();
    if (sym == null)
      return;
    popBinding();
  }

  public final void pushDecls (ScopeExp scope)
  {
    //shadowStack.push(null);
    for (Declaration decl = scope.firstDecl();
         decl != null;  decl = decl.nextDecl())
      push(decl);
  }

  public final void popDecls (ScopeExp scope)
  {
    for (Declaration decl = scope.firstDecl();
         decl != null;  decl = decl.nextDecl())
      pop(decl);
  }

  public void push (ScopeExp scope)
  {
    scope.outer = current_scope;
    if (! (scope instanceof ModuleExp))
      mustCompileHere();
    current_scope = scope;
    pushDecls(scope);
  }

  public void pop (ScopeExp scope)
  {
    popDecls(scope);
    current_scope = scope.outer;
  }

  ScopeExp current_scope;

}
