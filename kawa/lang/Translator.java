package kawa.lang;
import kawa.standard.Scheme;
import gnu.bytecode.Method;
import gnu.bytecode.Variable;
import gnu.mapping.*;
import gnu.expr.*;
import java.lang.reflect.Modifier;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;

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
  public Environment current_decls;
  ScopeExp current_scope;

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public ScopeExp currentScope() { return current_scope; }

  private Syntax current_syntax;
  private Object current_syntax_args;
  private static Expression errorExp = new ErrorExp ("unknown syntax error");
  String current_filename;
  int current_line;
  int current_column;

  // Global environment used to look for syntax/macros.
  private Environment env;

  public Translator (Environment env)
  {
    this.env = env;
    current_decls = new Environment();
  }

  public Translator ()
  {
    this (Environment.user());
  }

  final Expression rewrite_car (Pair pair)
  {
    Object car = pair.car;
    if (pair instanceof PairWithPosition)
      return rewrite_with_position (car, (PairWithPosition) pair);
    else
      return rewrite (car);
  }

  /**
   * Apply a Syntax object.
   * @param syntax the Syntax object whose rewrite method we call
   * @param args the syntax arguments (the cdr of the syntax form)
   * @return the re-written form as an Expression object
   */
  Expression apply_rewrite (Syntax syntax, Object args)
  {
    Syntax save_syntax = syntax;
    Object save_args = args;
    current_syntax = syntax;
    current_syntax_args = args;
    Expression exp = errorExp;
    try
      {
	exp = syntax.rewrite (args, this);
      }
    finally
      {
	current_syntax = save_syntax;
	current_syntax_args = save_args;
      }
    return exp;
  }

  /** Count of errors seen (at compile time). */
  public int errors;

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public Expression syntaxError (String message)
  {
    errors++;
    OutPort err = OutPort.errDefault();
    if (current_line > 0)
      {
	if (current_filename != null)
	  err.print (current_filename);
	err.print (':');
	err.print (current_line);
	if (current_column > 1)
	  {
	    err.print (':');
	    err.print (current_column);
	  }
	err.print (": ");
      }
    err.println (message);
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
	obj = current_decls.get (sym);
	Binding binding = null;
        if (obj != null)
	  {
	    // Hygenic macro expansion may bind a renamed (uninterned) symbol
	    // to the original symbol.  Here, use the original symbol.
	    if (obj instanceof String)
	      binding = env.lookup((String) obj);
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
    Object cdr = p.cdr;

    Expression func = rewrite_car (p);
    Object proc = null;
    ReferenceExp ref = null;

    if (func instanceof ReferenceExp)
      {
	ref = (ReferenceExp) func;
	if (ref.getBinding() == null)
	  {
	    proc = getBinding(ref.getName());
	    if (proc instanceof Syntax)
	      return apply_rewrite ((Syntax) proc, cdr);
	    if (proc instanceof Inlineable)
	      func = new QuoteExp(proc);
	  }
      }

    int cdr_length = List.length (cdr);

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
    if (proc != null && proc instanceof Procedure)
      {
	if (proc instanceof AutoloadProcedure)
	  {
	    try
	      {
		proc = ((AutoloadProcedure) proc).getLoaded();
	      }
	    catch (RuntimeException ex)
	      {
		break tryDirectCall;
	      }
	    if (proc == null || ! (proc instanceof Procedure))
	      break tryDirectCall;
	  }
	Class procClass = proc.getClass();
	if (procClass.getClassLoader() != null)
	  break tryDirectCall;
	try
	  {
	    java.lang.reflect.Method[] meths = procClass.getDeclaredMethods();
	    java.lang.reflect.Method best = null;
	    Class[] bestTypes = null;
	    String name = ((Procedure) proc).getName();
	    if (name == null)
	      break tryDirectCall;
	    String mangledName = Compilation.mangleName(name);
	    for (int i = meths.length;  --i >= 0; )
	      {
		java.lang.reflect.Method meth = meths[i];
		int mods = meth.getModifiers();
		if ((mods & (Modifier.STATIC|Modifier.PUBLIC))
		    != (Modifier.STATIC|Modifier.PUBLIC))
		  continue;
		String mname = meth.getName();
		if (! mname.equals("apply") && ! mname.equals(mangledName))
		  continue;
		Class[] ptypes = meth.getParameterTypes();
		if (ptypes.length != cdr_length)
		  continue;
		// In the future, we ma try to find the "best" match.
		if (best != null)
		  return syntaxError("ambiguous inline for call to "+name
				     + " (in class "+procClass.getName()+")");
		best = meth;
		bestTypes = ptypes;
	      }
	    if (best != null)
	      {
		ClassType procType = ClassType.make(procClass.getName());
		Type[] argTypes = new Type[bestTypes.length];
		for (int i = argTypes.length;  --i >= 0; )
		  argTypes[i] = Type.make(bestTypes[i]);
		gnu.bytecode.Method method
		  = procType.addMethod(best.getName(), best.getModifiers(),
				       argTypes,
				       Type.make(best.getReturnType()));
		func = new QuoteExp(new PrimProcedure(method));
	      }
	  }
	catch (SecurityException ex)
	  {
	  }
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
	Object binding = current_decls.get (name);

	// Hygenic macro expansion may bind a renamed (uninterned) symbol
	// to the original symbol.  Here, use the original symbol.
	if (binding != null && binding instanceof String)
	  return new ReferenceExp ((String) binding);
	return new ReferenceExp (name, (Declaration) binding);
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

  /** Recursive helper method for rewrite_body.
   * Scan body for definitions, placing partially macro-expanded
   * expressions into forms.
   * If definitions were seen, return a LetExp containing the definitions.
   */

  LetExp scan_body (Object body, java.util.Vector forms, LetExp defs)
  {
    while (body != List.Empty)
      {
	if (! (body instanceof Pair))
	  {
	    forms.addElement (syntaxError ("body is not a proper list"));
	    return defs;
	  }
	Pair pair = (Pair) body;
	Object st = pair.car;
	for (;;)
	{
	  // Process st.
	  if (! (st instanceof Pair))
	    forms.addElement (st);
	  else
	    {
	      Pair st_pair = (Pair) st;
	      Object op = st_pair.car;
	      Syntax syntax = check_if_Syntax (op);
	      if (syntax != null && syntax instanceof Macro)
		{
		  st = ((Macro) syntax).expand (st_pair.cdr, this);
		  continue;
		}
	      else if (syntax == Scheme.beginSyntax)
		defs = scan_body (st_pair.cdr, forms, defs);
	      else if (syntax == Scheme.defineSyntax
		       && st_pair.cdr instanceof Pair
		       && ! (current_scope instanceof ModuleExp))
		{
		  Object name = ((Pair) st_pair.cdr).car;
		  if (defs == null)
		    defs = new LetExp (null);
		  if (name instanceof String)
		    defs.addDeclaration((String) name);
		  else if (name instanceof Pair)
		    {
		      Pair name_pair = (Pair) name;
		      if (name_pair.car instanceof String)
			defs.addDeclaration((String) name_pair.car);
		    }
		  forms.addElement (st);
		}
	      else
		forms.addElement (st);
	    }
	  break;
	}
	body = pair.cdr;
      }
    return defs;
  }

  /**
   * Re-write a Scheme <body> in S-expression format into internal form.
   */

  public Expression rewrite_body (Object exp)
  {
    java.util.Vector forms = new java.util.Vector(20);
    LetExp defs = scan_body (exp, forms, null);
    int nforms = forms.size();
    if (nforms == 0)
      return syntaxError ("body with no expressions");
    int ndecls;
    if (defs == null)
      ndecls = 0;
    else
      {
	ndecls = defs.countDecls();
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
    if (defs == null)
      return body;
    defs.body = body;
    pop(defs);
    return defs;
  }

  /**
   * Insert decl into current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void push (Declaration decl)
  {
    String sym = decl.symbol();
    Object old_decl = current_decls.get (sym);
    if (old_decl != null)
      decl.shadowed = old_decl;
    current_decls.put (sym, decl);
  }

  /** Remove this from Translator.current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void pop (Declaration decl)
  {
    String sym = decl.symbol();
    if (decl.shadowed == null)
      current_decls.remove (sym);
    else
      current_decls.put (sym, decl.shadowed);
  }

  public final void pushDecls (ScopeExp scope)
  {
    for (Variable var = scope.firstVar();  var != null;  var = var.nextVar())
      {
	// Don't make artificial variables visible.
	if (! var.isArtificial())
	  push((Declaration)var);
      }
  }

  public final void popDecls (ScopeExp scope)
  {
    for (Variable var = scope.firstVar();  var != null;  var = var.nextVar())
      {
	if (! var.isArtificial ())
	  pop((Declaration)var);
      }
  }

  public void push (ScopeExp scope)
  {
    scope.outer = current_scope;
    if (! (scope instanceof LambdaExp)) // which implies: outer != null
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
