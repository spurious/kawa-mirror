package gnu.expr;
import gnu.text.SourceMessages;

/** A very abstract "parser".
 * Converts some input representation to Expression trees. */

public class Parser
{
  /** If doing immediate evaluation. */
  public boolean immediate;

  public Parser(SourceMessages messages)
  {
    this.messages = messages;
  }

  /** This may not make sense, except for Lisp-like languages.
   * For those, 'input' an s-expression  from the reader. */
  public Expression parse (Object input)
  {
    throw new Error("unimeplemented parse");
  }

  public Interpreter getInterpreter() { return Interpreter.getInterpreter(); }

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public ModuleExp currentModule() { return current_scope.currentModule(); }

  /** Note that we have seen a construct that must be compiled, not evaluated.
   * If we are not inside a lambda (which is always compiled), but
   * only inside the outer-most ModuleExp, note that it must be compiled.
   */
  public void mustCompileHere ()
  {
    ScopeExp exp = current_scope;
    for (;; exp = exp.outer)
      {
	if (exp == null)
	  return;
	if (exp instanceof ModuleExp)
	  {
	    ((ModuleExp) exp).mustCompile = true;
	    return;
	  }
      }
  }

  public ScopeExp currentScope() { return current_scope; }

  //public abstract boolean popBinding();

  //public abstract void pushBinding(String name, Object value);

  public void push (ScopeExp scope)
  {
    scope.outer = current_scope;
    if (! (scope instanceof ModuleExp))
      mustCompileHere();
    current_scope = scope;
  }

  public void pop (ScopeExp scope)
  {
    current_scope = scope.outer;
  }

  public final void pop ()
  {
    pop(current_scope);
  }

  public Declaration lookup(String name, int namespace)
  {
    Interpreter interp = getInterpreter();
    for (ScopeExp scope = current_scope;  scope != null;  scope = scope.outer)
      {
	Declaration decl = scope.lookup(name, interp, namespace);
	if (decl != null)
	  return decl;
      }
    return null;
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

  protected ScopeExp current_scope;

  protected String current_filename;
  protected int current_line;
  protected int current_column;

  protected SourceMessages messages;
}
