package gnu.expr;
import gnu.bytecode.*;

/**
 * Abstract class for expressions that add local variable bindings.
 * @author	Per Bothner
 */

public abstract class ScopeExp extends Expression
{
  Declaration decls;
  Declaration last;

  Scope scope;

  public final Declaration firstDecl () { return decls; }

  public void add (Declaration decl)
  {
    if (last == null)
      decls = decl;
    else
      last.next = decl;
    last = decl;
    decl.context = this;
  }

  /** Add a Declaration at a specified position.
   */
  public void add (Declaration prev, Declaration decl)
  {
    if (prev == null)
      { // Put first
        decl.next = decls;
        decls = decl;
      }
    else
      {
        decl.next = prev.next;
        prev.next = decl;
      }
    if (last == prev)
      last = decl;
    decl.context = this;
  }

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
	if (exp instanceof LambdaExp)
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
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.name == sym)
	  return decl;
      }
    return null;
  }

  /** Lookup a declaration, create a non-defining declaration if needed. */
  public Declaration getNoDefine (String name)
  {
    Declaration decl = lookup(name);
    if (decl == null)
      {
	decl = addDeclaration(name);
	decl.setFlag(Declaration.NOT_DEFINING);
      }
    return decl;
  }

  /** Add a new Declaration, with a message if there is an existing one. */
  public Declaration getDefine (String name, char severity, Parser parser)
  {
    Declaration decl = lookup(name);
    if (decl == null)
      decl = addDeclaration(name);
    else if (decl.getFlag(Declaration.NOT_DEFINING))
      decl.setFlag(false, Declaration.NOT_DEFINING);
    else
      {
	StringBuffer sbuf = new StringBuffer(200);
	sbuf.append("duplicate definition for '");
	sbuf.append(name);
	int oldLine = decl.getLine();
	if (oldLine <= 0)
	  sbuf.append('\'');
	else
	  {
	    sbuf.append("\' (overrides ");
	    String oldFile = decl.getFile();
	    if (oldFile == null || oldFile.equals(parser.getFile()))
	      sbuf.append("line ");
	    else
	      {
		sbuf.append(oldFile);
		sbuf.append(':');
	      }
	    sbuf.append(oldLine);
	    int oldColumn = decl.getColumn();
	    if (oldColumn > 0)
	      {
		sbuf.append(':');
		sbuf.append(oldColumn);
	      }
	    sbuf.append(')');
	  }
	parser.error(severity, sbuf.toString());
	decl = addDeclaration(name);
      }
    return decl;
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
    add(decl);  // FIXME just use add
  }

  public int countDecls ()
  {
    int n = 0;
    for (Declaration decl = firstDecl(); decl != null; decl = decl.nextDecl())
      n++;
    return n;
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkScopeExp(this);
  }
}
