package gnu.expr;
import gnu.text.SourceMessages;

/** A very abstract "parser".
 * Converts some input representation to Expression trees. */

public abstract class Parser
{
  public Parser(SourceMessages messages)
  {
    this.messages = messages;
  }

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

  public SourceMessages getMessages() { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }
 
  public void error(char severity, String message)
  {
    messages.error(severity, current_filename, current_line, current_column,
		   message);
  }

  public void error(char severity, Declaration decl, String msg1, String msg2)
  {
    String filename = current_filename;
    int line = current_line;
    int column = current_column;
    int decl_line = decl.getLine();
    if (decl_line > 0)
      {
	filename = decl.getFile();
	line = decl_line;
	column = decl.getColumn();
      }
    messages.error(severity, filename, line, column,
		   msg1 + decl.getName() + msg2);
  }

  public final String getFile() { return current_filename; }
  public final int getLine() { return current_line; }
  public final int getColumn() { return current_column; }

  public void setFile(String filename) { current_filename = filename; }
  public void setLine(int line) { current_line = line; }
  public void setColumn(int column) { current_column = column; }

  public void setLine(String filename, int line, int column)
  {
    current_filename = filename;
    current_line = line;
    current_column = column;
  }

  ScopeExp current_scope;

  protected String current_filename;
  protected int current_line;
  protected int current_column;

  protected SourceMessages messages;
}
