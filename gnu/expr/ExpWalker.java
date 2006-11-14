package gnu.expr;
import gnu.text.SourceMessages;

/** Class for doing a tree-walk over an Expression tree. */

public class ExpWalker
  implements gnu.text.SourceLocator
{
  protected SourceMessages messages;
  Compilation comp;

  /** Get the Compilation associated with this walker. */
  public Compilation getCompilation () { return comp; }

  public SourceMessages getMessages () { return messages; }

  protected Expression walkExpression (Expression exp)
  {
    exp.walkChildren(this);
    return exp;
  }

  public void setContext (Compilation comp)
  {
    this.comp = comp;
    messages = comp.getMessages();
  }

  /** Call the walk method of argument Expression.
   * Could call Expression's walk directly, but this allows us to
   * interpose a method call on each Expression.  We use it to note the
   * Expression's line number.  Should not need to be overridden;
   * if you do, you may also want to override walkExps. */
  public Expression walk(Expression exp)
  {
    int line = exp.getLineNumber();
    if (messages != null && line > 0)
      {
	String saveFile = messages.getFileName();
	int saveLine = messages.getLineNumber();
	int saveColumn = messages.getColumnNumber();
	messages.setLine(exp.getFileName(), line, exp.getColumnNumber());
	Expression ret = exp.walk(this);
	messages.setLine(saveFile, saveLine, saveColumn);
	return ret;
      }
    return exp.walk(this);
  }

  protected Expression walkApplyExp (ApplyExp exp)
  {
    return walkExpression(exp);
  }
  
  protected Expression walkIfExp (IfExp exp)
  {
    return walkExpression(exp);
  }

  protected final void walkDeclarationType (Declaration decl)
  {
    Expression texp1 = decl.getTypeExp();
    if (texp1 != null)
      {
        Expression texp2 = texp1.walk(this);
        if (texp2 != texp1)
          decl.setTypeExp(texp2);
      }
  }

  protected final void walkDeclarationTypes (ScopeExp exp)
  {
    for (Declaration decl = exp.firstDecl(); decl != null;
         decl = decl.nextDecl())
      {
        walkDeclarationType(decl);
      }
  }

  protected Expression walkScopeExp (ScopeExp exp)
  {
    walkDeclarationTypes(exp);
    return walkExpression(exp);
  }

  protected Expression walkLetExp (LetExp exp) { return walkScopeExp(exp); }
  protected Expression walkLambdaExp (LambdaExp exp) { return walkScopeExp(exp); }
  protected Expression walkClassExp (ClassExp exp) { return walkLambdaExp(exp); }
  protected Expression walkObjectExp (ObjectExp exp) { return walkClassExp(exp); }
  protected Expression walkModuleExp (ModuleExp exp) { return walkLambdaExp(exp); }

  protected Expression walkSetExp (SetExp exp)
  {
    Declaration decl = exp.binding;
    boolean updateNeeded = decl != null && decl.value == exp.new_value;
    Expression ret = walkExpression(exp);
    // If the Declarations's value was that of our new_value,
    // and we modified/simplified the latter, update the former as well.
    if (updateNeeded && exp.isDefining())
      {
        decl.value = exp.new_value;
        if (exp.new_value instanceof LambdaExp)
          ((LambdaExp) exp.new_value).nameDecl = decl;
      }
    return ret;
  }

  //protected Expression walkSwitchExp (SwitchExp exp) { return walkExpression(exp); }
  protected Expression walkTryExp (TryExp exp) { return walkExpression(exp); }
  protected Expression walkBeginExp (BeginExp exp) { return walkExpression(exp); }
  protected Expression walkQuoteExp (QuoteExp exp) { return walkExpression(exp); }
  protected Expression walkReferenceExp (ReferenceExp exp)
  { return walkExpression(exp); }
  protected Expression walkThisExp (ThisExp exp) { return walkReferenceExp(exp); }
  protected Expression walkSynchronizedExp (SynchronizedExp exp)
    { return walkExpression(exp); }

  protected Expression walkBlockExp(BlockExp exp) { return walkExpression(exp); }
  protected Expression walkExitExp(ExitExp exp) { return walkExpression(exp); }
  protected Expression walkFluidLetExp(FluidLetExp exp)
  {
    return walkLetExp(exp);
  }
  protected Expression walkLangExp (LangExp exp)
  { return walkExpression(exp); }

  protected LambdaExp currentLambda = null;

  /** If exitValue is set to non-null, the walk stops. */
  Object exitValue = null;

  public final LambdaExp getCurrentLambda() { return currentLambda; }

  public Expression[] walkExps (Expression[] exps)
  {
    return walkExps(exps, exps.length);
  }

  /** Call walk on the Expressions in an array.
   * However, the walk method is inlined for speed.
   */
  public Expression[] walkExps (Expression[] exps, int n)
  {
    String saveFile;
    int saveLine;
    int saveColumn;
    if (messages != null)
      {
	saveFile = messages.getFileName();
	saveLine = messages.getLineNumber();
	saveColumn = messages.getColumnNumber();
      }
    else
      {
	saveFile = null;
	saveLine = 0;
	saveColumn = 0;
      }
    boolean mustRestore = false;
    for (int i = 0;  i < n && exitValue == null;  i++)
      {
	Expression exp = exps[i];
	int line = exp.getLineNumber();
	if (messages != null && line > 0)
	  {
	    messages.setLine(exp.getFileName(), line, exp.getColumnNumber());
	    mustRestore = true;
	  }
	else if (mustRestore)
	  {
	    messages.setLine(saveFile, saveLine, saveColumn);
	    mustRestore = false;
	  }
	exps[i] = walk(exp);
      }
    if (mustRestore)
      messages.setLine(saveFile, saveLine, saveColumn);
    return exps;
  }

  public void walkDefaultArgs (LambdaExp exp)
  {
    if (exp.defaultArgs != null)
      exp.defaultArgs = walkExps(exp.defaultArgs);
  }

  public void error(char kind, String message)
  {
    if (kind == 'w' && comp.getBooleanOption("warn-as-error", false))
      kind = 'e';
    if (messages != null)
      messages.error(kind, message);
    else
      new Error("internal error: "+message);
  }

  public Expression noteError (String message)
  {
    if (messages != null)
      messages.error('e', message);
    return new ErrorExp (message);
  }

  public final String getFileName() { return messages.getFileName(); }
  public final int getLineNumber() { return messages.getLineNumber(); }
  public final int getColumnNumber() { return messages.getColumnNumber(); }
  public String getPublicId() { return messages.getPublicId(); }
  public String getSystemId() { return messages.getSystemId(); }
  /** Normally same as getSystemId. */

  public boolean isStableSourceLocation() { return false; }

  public void setFile(String filename) { messages.setFile(filename); }
  public void setLine(int line) { messages.setLine(line); }
  public void setColumn(int column) { messages.setColumn(column); }

  public void setLine(String filename, int line, int column)
  {
    messages.setLine(filename, line, column);
  }
}
