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

  private Scope scope;

  public Declaration firstDecl () { return decls; }

  public Scope getVarScope ()
  {
    Scope sc = scope;
    if (sc == null)
      scope = sc = new Scope();
    return sc;
  }

  /** Clear bytecode resources for the ScopeExp.
   * This potentially allows Kawa to generate code for the same (inlined,
   * shared) ScopeExp multiple times - though we're not making use of that yet.
   */
  public void popScope (CodeAttr code)
  {
    for (Declaration decl = firstDecl(); decl != null; decl = decl.nextDecl())
      decl.var = null;
    code.popScope();
    scope = null;
  }

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

  /** Replace the <code>prev.next</code> by <code>newDecl</code>.
   * If <code>prev==null</code>, replace the first decl. */
  public void replaceFollowing (Declaration prev, Declaration newDecl)
  {
    Declaration oldDecl;
    if (prev == null)
      {
	oldDecl = decls;
	decls = newDecl;
      }
    else
      {
	oldDecl = prev.next;
	prev.next = newDecl;
      }
    newDecl.next = oldDecl.next;
    if (last == oldDecl)
      last = newDecl;
    oldDecl.next = null;
    newDecl.context = this;
  }

  public void remove (Declaration decl)
  {
    Declaration prev = null;
    for (Declaration cur = firstDecl(); cur != null; cur = cur.nextDecl())
      {
	if (cur == decl)
	  {
	    remove(prev, decl);
	    return;
	  }
	prev = cur;
      }
  }

  public void remove (Declaration prev, Declaration decl)
  {
    if (prev == null)
      decls = decl.next;
    else
      prev.next = decl.next;
    if (last == decl)
      last = prev;
  }

  public ScopeExp () { }

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

  public ModuleExp currentModule ()
  {
    ScopeExp exp = this;
    for (;; exp = exp.outer)
      {
	if (exp == null)
	  return null;
	if (exp instanceof ModuleExp)
	  return (ModuleExp) exp;
      }
  }

  /**
   * Find a Declaration by name.
   * @param sym the (interned) name of the Declaration sought
   * @return the matching Declaration, if found;  otherwise null
   */
  public Declaration lookup (Object sym)
  {
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.symbol == sym)
	  return decl;
      }
    return null;
  }

  public Declaration lookup (Object sym, Language language, int namespace)
  {
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.symbol == sym
	    && (language.getNamespaceOf(decl) & namespace) != 0)
	  return decl;
      }
    return null;
  }

  /** Lookup a declaration, create a non-defining declaration if needed. */
  public Declaration getNoDefine (Object name)
  {
    Declaration decl = lookup(name);
    if (decl == null)
      {
	decl = addDeclaration(name);
	decl.flags |= Declaration.NOT_DEFINING | Declaration.IS_UNKNOWN;
      }
    return decl;
  }

  /** Add a new Declaration, with a message if there is an existing one. */
  public Declaration getDefine (Object name, char severity, Compilation parser)
  {
    Declaration decl = lookup(name);
    if (decl == null)
      decl = addDeclaration(name);
    else if ((decl.flags & (Declaration.NOT_DEFINING | Declaration.IS_UNKNOWN))
	     != 0)
      decl.flags &= ~ (Declaration.NOT_DEFINING|Declaration.IS_UNKNOWN);
    else
      {
        parser.error(severity, "duplicate definition of '"+name+"' here");
        parser.error(severity, decl, "previous definition of '", "' here");
	decl = addDeclaration(name);
      }
    return decl;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   */
  public final Declaration addDeclaration (Object name)
  {
    Declaration decl = new Declaration (name);
    add(decl);
    return decl;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   * @param type type of the new Declaration.
   */
  public final Declaration addDeclaration (Object name, Type type)
  {
    Declaration decl = new Declaration (name, type);
    add(decl);
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

  public static int nesting (ScopeExp sc)
  {
    int n = 0;
    while (sc != null)
      {
	sc = sc.outer;
	n++;
      }
    return n;
  }

  /** Size of evalFrame to allocate in interpreter. */
  protected int frameSize;

  /** Calculate offset and frameSize needed by interpreter. */
  protected void setIndexes ()
  {
    int i = 0;
    for (Declaration decl = firstDecl(); decl != null; decl = decl.nextDecl())
      {
        decl.evalIndex = i++;
      }
    frameSize = i;
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkScopeExp(this);
  }

  public String toString() { return getClass().getName()+"#"+id; }

  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  public int id = ++counter;
}
