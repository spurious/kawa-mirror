package kawa.lang;
import kawa.standard.Scheme;
import gnu.bytecode.Method;
import gnu.bytecode.Variable;
import gnu.mapping.*;
import gnu.expr.*;
import java.lang.reflect.Modifier;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.text.SourceMessages;
import gnu.kawa.util.*;

/** Used to translate from source to Expression.
 * The result has macros expanded, lexical names bound, etc, and is
 * ready for code generation.
 * This is sometimes called a "compilation environment",
 * but we modify it as we go along - there is a single Translator for
 * each top-level form.
 */

public class Translator extends Object
{
  // Map name to Declaration.
  public Environment environ;
  ScopeExp current_scope;

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public ScopeExp currentScope() { return current_scope; }

  /** Return true if decl is lexical and not fluid. */
  public boolean isLexical (Declaration decl)
  {
    if (decl == null)
      return false;
    if (! decl.isFluid())
      return true;
    ScopeExp scope = currentScope();
    ScopeExp context = decl.getContext();
    for (;; scope = scope.outer)
      {
	if (scope == null)
	  return false;
	if (scope == context)
	  return true;
	if (scope instanceof LambdaExp
	    && ! ((LambdaExp) scope).getInlineOnly())
	  return false;
      }
  }

  private static Expression errorExp = new ErrorExp ("unknown syntax error");
  String current_filename;
  int current_line;
  int current_column;

  // Global environment used to look for syntax/macros.
  private Environment env;

  public Translator (Environment env, SourceMessages messages)
  {
    this.env = env;
    environ = new Environment();
    this.messages = messages;
  }

  public Translator (Environment env)
  {
    this.env = env;
    environ = new Environment();
    messages = new SourceMessages();
  }

  public Translator ()
  {
    this (Environment.user());
  }

  public final Environment getGlobalEnvironment() { return env; }

  final Expression rewrite_car (Pair pair)
  {
    Object car = pair.car;
    if (pair instanceof PairWithPosition)
      return rewrite_with_position (car, (PairWithPosition) pair);
    else
      return rewrite (car);
  }

  Syntax currentSyntax;
  public Syntax getCurrentSyntax() { return currentSyntax; }

  /**
   * Apply a Syntax object.
   * @param syntax the Syntax object whose rewrite method we call
   * @param form the syntax form (including the macro name)
   * @return the re-written form as an Expression object
   */
  Expression apply_rewrite (Syntax syntax, Pair form)
  {
    Expression exp = errorExp;
    Syntax saveSyntax = currentSyntax;
    currentSyntax = syntax;
    try
      {
	exp = syntax.rewriteForm(form, this);
      }
    finally
      {
        currentSyntax = saveSyntax;
      }
    return exp;
  }

  SourceMessages messages;

  public SourceMessages getMessages() { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }
 
  public void error(char severity, String message)
  {
    messages.error(severity, current_filename, current_line, current_column,
		   message);
  }


  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public Expression syntaxError (String message)
  {
    error('e', message);
    return new ErrorExp (message);
  }

  /** Enter a global definition.
   * This allows macro definitions to be used in the same Translation
   * as the define-syntax.
   */

  public void addGlobal (String name, Object value)
  {
    env.put (name, value);
  }

  /** Note that we have seen a construct that must be compiled, not evaluated.
   * If we are not inside a lambda (which is always compiled), but
   * only inside the outer-most ModuleExp, note that it must be compiled. */
  public void mustCompileHere ()
  {
    LambdaExp lambda = currentLambda ();
    if (lambda instanceof ModuleExp)
      ((ModuleExp)lambda).mustCompile = true;
  }

  Object getBinding (Object obj)
  {
    if (obj instanceof String)
      {
	String sym = (String) obj;
	obj = environ.get (sym);

        if (obj instanceof Syntax)
          return obj;
	Binding binding = null;
        if (obj != null)
	  {
	    // Hygenic macro expansion may bind a renamed (uninterned) symbol
	    // to the original symbol.  Here, use the original symbol.
	    if (obj instanceof String)
	      binding = env.lookup((String) obj);
	    else if (obj instanceof Declaration
		     && ! isLexical((Declaration) obj))
	      obj = null;
	  }
	else
	  binding = env.lookup(sym);
	if (binding != null && binding.isBound())
	  return binding.get();
	return null;
      }
     return obj;
  }

  /** Check if Object is Syntax, or bound to Syntax.
   * @param obj the value to check
   * @return the Syntax bound to obj, or null.
   */
  public Syntax check_if_Syntax (Object obj)
  {
    obj = getBinding(obj);
    if (obj instanceof Syntax)
      return (Syntax) obj;
    return null;
  }

  public Expression rewrite_pair (Pair p)
  {
    if (p.car instanceof Syntax)
      return apply_rewrite((Syntax) p.car, p);
    Object cdr = p.cdr;

    Expression func = rewrite_car (p);
    Object proc = null;
    ReferenceExp ref = null;

    if (func instanceof ReferenceExp)
      {
	ref = (ReferenceExp) func;
        Declaration decl = ref.getBinding();
	if (decl == null)
	  {
            String name = ref.getName();
            Binding binding = env.lookup((String) name);
	    if (binding != null && binding.isBound())
              proc = binding.get();
	    if (proc instanceof Syntax)
	      return apply_rewrite ((Syntax) proc, p);
            if (proc instanceof AutoloadProcedure)
              {
                try
                  {
                    proc = ((AutoloadProcedure) proc).getLoaded();
                  }
                catch (RuntimeException ex)
                  {
                    proc = null;
                  }
              }
	    if (proc instanceof Inlineable)
	      func = new QuoteExp(proc);
	  }
        else if (decl instanceof Syntax)
          return apply_rewrite ((Syntax) decl, p);
      }

    int cdr_length = LList.length (cdr);

    if (func instanceof QuoteExp)
      {
	proc = ((QuoteExp) func).getValue();
	if (proc instanceof Procedure)
	  {
	    String msg = WrongArguments.checkArgCount((Procedure) proc,
						      cdr_length);
	    if (msg != null)
	      return syntaxError(msg);
	  }
      }

    Expression[] args = new Expression[cdr_length];

    for (int i = 0; i < cdr_length; i++)
      {
	Pair cdr_pair = (Pair) cdr;
	args[i] = rewrite_car (cdr_pair);
	cdr = cdr_pair.cdr;
      }

  tryDirectCall:
    if (proc != null && proc instanceof Procedure
        && ! (proc instanceof Inlineable))
      {
	if (proc instanceof AutoloadProcedure)
	  {
	    try
	      {
		proc = ((AutoloadProcedure) proc).getLoaded();
	      }
	    catch (RuntimeException ex)
	      {
                ex.printStackTrace(System.err);
		break tryDirectCall;
	      }
	    if (proc == null || ! (proc instanceof Procedure)
                || proc instanceof Inlineable)
	      break tryDirectCall;
	  }

        PrimProcedure pproc
          = PrimProcedure.getMethodFor((Procedure) proc, args);
        if (pproc != null)
          func = new QuoteExp(pproc);
      }

    return new ApplyExp (func, args);
  }

  /**
   * Re-write a Scheme expression in S-expression format into internal form.
   */
  public Expression rewrite (Object exp)
  {
    if (exp instanceof PairWithPosition)
      return rewrite_with_position (exp, (PairWithPosition) exp);
    else if (exp instanceof Pair)
      return rewrite_pair ((Pair) exp);
    else if (exp instanceof String)
      {
	String name = (String) exp;
	Object binding = environ.get (name);
	// Hygenic macro expansion may bind a renamed (uninterned) symbol
	// to the original symbol.  Here, use the original symbol.
	if (binding != null && binding instanceof String)
	  return new ReferenceExp ((String) binding);
	Declaration decl = null;
        if (binding instanceof Declaration) // ?? FIXME
          {
            decl = (Declaration) binding;
            if (! isLexical(decl))
              decl = null;
          }
	return new ReferenceExp (name, decl);
      }
    else if (exp instanceof Expression)
      return (Expression) exp;
    else
      return new QuoteExp (exp);
  }

  public Expression rewrite_with_position (Object exp, PairWithPosition pair)
  {
    String save_filename = current_filename;
    int save_line = current_line;
    int save_column = current_column;
    Expression result;
    try
      {
	String exp_file = pair.getFile ();
	int exp_line = pair.getLine ();
	int exp_column = pair.getColumn ();
	current_filename = exp_file;
        current_line = exp_line;
	current_column = exp_column;
	if (exp == pair)
	  result = rewrite_pair (pair);  // To avoid a cycle
	else
	  result = rewrite (exp);
	if (result.getFile () == null)
	  result.setFile (exp_file);
	if (result.getLine () == 0)
	  result.setLine (exp_line, exp_column);
      }
    finally
      {
	current_filename = save_filename;
	current_line = save_line;
	current_column = save_column;
      }
    return result;
  }

  boolean scan_form (Object st, java.util.Vector forms, ScopeExp defs)
  {
    // Process st.
    if (! (st instanceof Pair))
      forms.addElement (st);
    else
      {
        Pair st_pair = (Pair) st;
        Object op = st_pair.car;
        Syntax syntax = check_if_Syntax (op);
        if (syntax == null)
          forms.addElement(st);
        else if (! syntax.scanForDefinitions(st_pair, forms, defs, this))
          return false;
      }
    return true;
  }

  /** Recursive helper method for rewrite_body.
   * Scan body for definitions, placing partially macro-expanded
   * expressions into forms.
   * If definitions were seen, return a LetExp containing the definitions.
   */

  public boolean scan_body (Object body, java.util.Vector forms, ScopeExp defs)
  {
    boolean result = true;
    while (body != LList.Empty)
      {
	if (! (body instanceof Pair))
	  {
	    forms.addElement (syntaxError ("body is not a proper list"));
	    return false;
	  }
	Pair pair = (Pair) body;
	Object st = pair.car;
        if (! scan_form (st, forms, defs))
          result = false;
	body = pair.cdr;
      }
    return result;
  }

  public static Pair makePair(Pair pair, Object car, Object cdr)
  {
    if (pair instanceof PairWithPosition)
      return new PairWithPosition((PairWithPosition) pair, car, cdr);
    return new Pair(car, cdr);
  }

  /**
   * Re-write a Scheme <body> in S-expression format into internal form.
   */

  public Expression rewrite_body (Object exp)
  {
    java.util.Vector forms = new java.util.Vector(20);
    LetExp defs = new LetExp(null);
    if (! scan_body (exp, forms, defs))
      return new ErrorExp("error while scanning in body");
    return rewrite_body(forms, defs);
  }

  public Expression rewrite_body (java.util.Vector forms, LetExp defs)
  {
    int nforms = forms.size();
    if (nforms == 0)
      return syntaxError ("body with no expressions");
    int ndecls = defs.countDecls();
    if (ndecls != 0)
      {
        Expression[] inits = new Expression[ndecls];
        for (int i = ndecls;  --i >= 0; )
          inits[i] = QuoteExp.nullExp;
        defs.inits = inits;
	push(defs);
      }
    Expression body;
    if (nforms == 1)
      body = rewrite (forms.elementAt(0));
    else
      {
	Expression[] exps = new Expression [nforms];
	for (int i = 0; i < nforms; i++)
	  exps[i] = rewrite (forms.elementAt(i));
	body = new BeginExp (exps);
      }
    if (ndecls == 0)
      return body;
    defs.body = body;
    pop(defs);
    return defs;
  }

  public ModuleExp module;

  public void finishModule(ModuleExp mexp, java.util.Vector forms)
  {
    module = mexp;
    int nforms = forms.size();
    int ndecls = mexp.countDecls();
    pushDecls(mexp);
    Expression body;
    if (nforms == 1)
      body = rewrite(forms.elementAt(0));
    else
      {
	Expression[] exps = new Expression [nforms];
	for (int i = 0; i < nforms; i++)
	  exps[i] = rewrite(forms.elementAt(i));
	body = new BeginExp (exps);
      }
    mexp.body = body;
    pop(mexp);
    /* DEBUGGING:
    OutPort err = OutPort.errDefault ();
    err.print ("[Re-written expression for load/compile: ");
    mexp.print (err);
    //err.print ("\nbefore load<"+mod.getClass().getName()+">");
    err.println();
    err.flush();
    */
  }

  /** Used to remember shadowed bindings in environ.
   * For each binding, we push <old binding>, <name>.
   * For each new scope, we push <null>. */
  java.util.Stack shadowStack = new java.util.Stack();

  /** Note a new binding, remembering old binding in the shadowStack. */
  public void pushBinding(String name, Object value)
  {
    Object old = environ.put(name, value);
    shadowStack.push(old);
    shadowStack.push(name);
  }

  public boolean popBinding()
  {
    Object name = shadowStack.pop();
    if (name == null)
        return false;
    Object old = shadowStack.pop();
    if (old == null)
      environ.remove(name);
    else
      environ.put(name, old);
    return true;
  }

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

}
