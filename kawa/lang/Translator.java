package kawa.lang;
import kawa.standard.Scheme;
import kawa.standard.StaticFieldConstraint;
import gnu.bytecode.Method;
import gnu.bytecode.Variable;
import gnu.mapping.*;
import gnu.expr.*;
import java.lang.reflect.Modifier;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.text.SourceMessages;
import gnu.lists.*;
import gnu.kawa.lispexpr.LispInterpreter;

/** Used to translate from source to Expression.
 * The result has macros expanded, lexical names bound, etc, and is
 * ready for code generation.
 * This is sometimes called a "compilation environment",
 * but we modify it as we go along - there is a single Translator for
 * each top-level form.
 */

public class Translator extends Parser
{
  // Map name to Declaration.
  public Environment environ;

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

  // Global environment used to look for syntax/macros.
  private Environment env;

  public Translator (Environment env, SourceMessages messages)
  {
    super(messages);
    this.env = env;
    environ = new Environment();
  }

  public Translator (Environment env)
  {
    super(new SourceMessages());
    this.env = env;
    environ = new Environment();
  }

  public Translator ()
  {
    this (Environment.user());
  }

  public final Environment getGlobalEnvironment() { return env; }

  public Expression parse (Object input)
  {
    return rewrite(input);
  }

  final Expression rewrite_car (Pair pair, boolean function)
  {
    Object car = pair.car;
    if (pair instanceof PairWithPosition)
      return rewrite_with_position (car, function, (PairWithPosition) pair);
    else
      return rewrite (car, function);
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

  /**
   * @param function true if obj is in function-call position (i.e. first).
   */
  Object getBinding (Object obj, boolean function)
  {
    if (obj instanceof String || obj instanceof Binding)
      {
	String name = obj.toString();
	// Same logic as in rewrite(Object,boolean)..
	Binding binding = environ.lookup(name);
	if (binding == null)
	  binding = obj instanceof Binding ? (Binding) obj : env.lookup(name);
	else if (binding.isBound())
	  {
	    Object val1 = binding.getValue();
            // Check for hygiene re-naming - see SyntaxRule.execute_template.
	    if (val1 instanceof String)
	      {
		name = (String) val1;
		binding = env.lookup(name);
	      }
	  }
	if (binding == null)
	  obj = null;
	else if (function && getInterpreter().hasSeparateFunctionNamespace())
	  obj = binding.getFunctionValue(null);
        else if (binding.isBound())
          obj = binding.getValue();
        else
          obj = null;
        if (obj instanceof Syntax)
          return obj;
	if (obj instanceof Declaration)
	  {
	    Expression dval = ((Declaration) obj).getValue();
	    if (dval instanceof QuoteExp)
	      return ((QuoteExp) dval).getValue();
	  }
	binding = null;
        if (obj != null)
	  {
            if (obj instanceof Declaration
		     && ! isLexical((Declaration) obj))
	      obj = null;
	  }
	else
	  binding = env.lookup(name);
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
    obj = getBinding(obj, true);
    if (obj instanceof Syntax)
      return (Syntax) obj;
    return null;
  }

  public Expression rewrite_pair (Pair p)
  {
    if (p.car instanceof Syntax)
      return apply_rewrite((Syntax) p.car, p);
    Object cdr = p.cdr;

    Expression func = rewrite_car (p, true);
    Object proc = null;
    ReferenceExp ref = null;
    if (func instanceof ReferenceExp)
      {
	ref = (ReferenceExp) func;
        Declaration decl = ref.getBinding();
	if (decl == null)
	  {
            String name = ref.getName();
            Binding binding = env.lookup(name);
	    if (binding != null)
	      if (getInterpreter().hasSeparateFunctionNamespace())
		proc = binding.getFunctionValue(null);
	      else
		proc = binding.getValue();
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
	  }
        else if (decl.getFlag(Declaration.IS_SYNTAX))
          return apply_rewrite ((Syntax) decl.getConstantValue(), p);

        if (proc != null && proc instanceof Procedure
            && ! immediate && ref.getBinding() == null)
          {
            Procedure pproc = (Procedure) proc;

            Class procClass = PrimProcedure.getProcedureClass(pproc);
            gnu.bytecode.Field procField;
	    String pname = pproc.getName();
            if (procClass != null && pname != null)
              {
                ClassType procType = (ClassType) Type.make(procClass);
                procField
		  = procType.getDeclaredField(Compilation.mangleName(pname));
              }
            else
              procField = null;
            if (procField != null)
              {
                int fflags = procField.getModifiers();
                if ((fflags & Access.STATIC) != 0)
                  {
                    Declaration fdecl
                      = new Declaration(pproc.getName(), procField);
                    fdecl.noteValue(new QuoteExp(pproc));
                    ref.setBinding(fdecl);
                    if ((fflags & Access.FINAL) != 0)
                      fdecl.setFlag(Declaration.IS_CONSTANT);
                  }
              }
          }

	ref.setProcedureName(true);
	if (getInterpreter().hasSeparateFunctionNamespace())
	  func.setFlag(ReferenceExp.PREFER_BINDING2);
      }

    int cdr_length = LList.listLength(cdr, false);

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
	args[i] = rewrite_car (cdr_pair, false);
	cdr = cdr_pair.cdr;
      }

    return new ApplyExp (func, args);
  }

  /**
   * Re-write a Scheme expression in S-expression format into internal form.
   */
  public Expression rewrite (Object exp)
  {
    return rewrite(exp, false);
  }

  /**
   * Re-write a Scheme expression in S-expression format into internal form.
   */
  public Expression rewrite (Object exp, boolean function)
  {
    if (exp instanceof PairWithPosition)
      return rewrite_with_position (exp, function, (PairWithPosition) exp);
    else if (exp instanceof Pair)
      return rewrite_pair ((Pair) exp);
    else if (exp instanceof String || exp instanceof Binding)
      {
	String name = exp.toString();
	// Same logic as in getBinding.  The duplication is difficult to avoid,
	// because there are two results: Binding and name may is changed.
	Binding binding = environ.lookup(name);
        if (binding == null)
	  binding = exp instanceof Binding ? (Binding) exp : env.lookup(name);
        else if (binding.isBound())
          {
            // Check for hygiene re-naming - see SyntaxRule.execute_template.
            Object val1 = binding.getValue();
            if (val1 instanceof String)
              {
                name = (String) val1;
                binding = env.lookup(name);
              }
          }
        Object value;
	if (binding == null)
	  value = null;
	else if (function && getInterpreter().hasSeparateFunctionNamespace())
	  value = binding.getFunctionValue(null);
        else if (binding.isBound())
          value = binding.getValue();
        else
          value = null;
        boolean separate = getInterpreter().hasSeparateFunctionNamespace();
	Declaration decl = null;
        if (value instanceof Declaration) // ?? FIXME
          {
            decl = (Declaration) value;
            if (! isLexical(decl)
                || (separate && decl.isProcedureDecl()))
              decl = null;
          }
        else if (! immediate && value instanceof Named)
          {
            if (value instanceof AutoloadProcedure)
              {
                try
                  {
                    value = ((AutoloadProcedure) value).getLoaded();
                  }
                catch (RuntimeException ex)
                  {
                  }
              }
            Named proc = (Named) value;
            Constraint constraint = binding.getConstraint();
            if (constraint instanceof StaticFieldConstraint)
              {
                StaticFieldConstraint fconstraint
                  = (StaticFieldConstraint) constraint;
                String fname = fconstraint.getName();
                ClassType t = fconstraint.getDeclaringClass();
                gnu.bytecode.Field procField = t.getDeclaredField(fname);
                if (procField != null && procField.getStaticFlag())
                  {
                    int fflags = procField.getModifiers();
                    decl = new Declaration(proc.getName(), procField);
                    decl.noteValue(new QuoteExp(proc));
                    if ((fflags & Access.FINAL) != 0)
                      decl.setFlag(Declaration.IS_CONSTANT);
                    if (value instanceof Syntax)
                      decl.setFlag(Declaration.IS_SYNTAX);
                  }
              }
            else
              {
                decl = Declaration.getDeclaration(proc);
              }
          }
	ReferenceExp rexp = new ReferenceExp (name, decl);
	if (separate)
	  rexp.setFlag(ReferenceExp.PREFER_BINDING2);
	return rexp;
      }
    else if (exp instanceof Expression)
      return (Expression) exp;
    else
      return new QuoteExp (exp);
  }

  public static void setLine(Declaration decl, Object pair)
  {
    if (pair instanceof PairWithPosition)
      {
	PairWithPosition declPos = (PairWithPosition) pair;
	decl.setFile(declPos.getFile());
	decl.setLine(declPos.getLine(), declPos.getColumn());
      }
  }

  PairWithPosition positionPair;

  /** Note current line number position from a PairWithPosition.
   * Return an object to pass to popPositionOf.
   */
  public Object pushPositionOf(Object pair)
  {
    if (! (pair instanceof PairWithPosition))
      return null;
    PairWithPosition ppair = (PairWithPosition) pair;
    Object saved = positionPair;
    if (positionPair == null
	|| positionPair.getFile() != current_filename
	|| positionPair.getLine() != current_line
	|| positionPair.getColumn() != current_column)
      {
	saved = PairWithPosition.make(Special.eof, positionPair,
				      current_filename,
				      current_line, current_column);
      }
    setLine(pair);
    positionPair = ppair;
    return saved;
  }

  /** Restore  line number position from a previous pushPositionOf.
   * @param saved value returned by matching pushPositionOf.
   */
  public void popPositionOf(Object saved)
  {
    if (saved == null)
      return;
    setLine(saved);
    positionPair = (PairWithPosition) saved;
    if (positionPair.car == Special.eof)
      positionPair = (PairWithPosition) positionPair.cdr;
  }


  public void setLine (Object pair)
  {
    if (pair instanceof PairWithPosition)
      {
	PairWithPosition pos = (PairWithPosition) pair;
	setLine(pos.getFile(), pos.getLine(), pos.getColumn());
      }
  }

  /** Extract a type from the car fo a pair. */
  public Type exp2Type(Pair typeSpecPair)
  {
    Object saved = pushPositionOf(typeSpecPair);
    try
      {
	Expression texp = rewrite_car(typeSpecPair, false);
	if (texp instanceof ErrorExp)
	  return null;
	Type type = getInterpreter().getTypeFor(texp);
	 if (type == null)
	   {
	     if (texp instanceof ReferenceExp)
	       error('e', "unknown type name '"
		     + ((ReferenceExp) texp).getName() + '\'');
	     else
	       error('e',
		 "invalid type spec (must be \"type\" or 'type or <type>)");
	   }
	 return type;
      }
    finally
      {
	popPositionOf(saved);
      }
  }

  public Expression rewrite_with_position (Object exp, boolean function,
                                           PairWithPosition pair)
  {
    Object saved = pushPositionOf(pair);
    Expression result;
    try
      {
	if (exp == pair)
	  result = rewrite_pair (pair);  // To avoid a cycle
	else
	  result = rewrite (exp, function);
	if (result.getFile () == null)
	  result.setFile(current_filename);
	if (result.getLine () == 0)
	  result.setLine (current_line, current_column);
      }
    finally
      {
	popPositionOf(saved);
      }
    return result;
  }

  public boolean scan_form (Object st, java.util.Vector forms, ScopeExp defs)
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
	else
	  {
	    String save_filename = current_filename;
	    int save_line = current_line;
	    int save_column = current_column;
	    try
	      {
		setLine(st_pair);
		if (! syntax.scanForDefinitions(st_pair, forms, defs, this))
		  return false;
	      }
	    finally
	      {
		current_filename = save_filename;
		current_line = save_line;
		current_column = save_column;
	      }
	  }
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
    Expression body = makeBody(forms);
    if (ndecls == 0)
      return body;
    defs.body = body;
    pop(defs);
    return defs;
  }

  /** Combine a list of zero or more expression forms info a "body". */
  public Expression makeBody(java.util.Vector forms)
  {
    int nforms = forms.size();
    if (nforms == 0)
      return QuoteExp.voidExp; 
   else if (nforms == 1)
      return rewrite (forms.elementAt(0));
    else
      {
	Expression[] exps = new Expression [nforms];
	for (int i = 0; i < nforms; i++)
	  exps[i] = rewrite (forms.elementAt(i));
	return ((LispInterpreter) getInterpreter()).makeBody(exps);
      }
  }

  ModuleExp module;

  public final ModuleExp getModule() { return module; }

  public void finishModule(ModuleExp mexp, java.util.Vector forms)
  {
    boolean moduleStatic = mexp.isStatic();
    for (Declaration decl = mexp.firstDecl();
	 decl != null;  decl = decl.nextDecl())
      {
	if (decl.getFlag(Declaration.NOT_DEFINING))
	  {
	    String msg1 = "'";
	    String msg2
	      = (decl.getFlag(Declaration.EXPORT_SPECIFIED)
		 ? "' exported but never defined"
		 : decl.getFlag(Declaration.STATIC_SPECIFIED)
		 ? "' declared static but never defined"
		 : "' declared but never defined");
	    error('e', decl, msg1, msg2);
	  }
	if (mexp.getFlag(ModuleExp.EXPORT_SPECIFIED))
	  {
	    if (decl.getFlag(Declaration.EXPORT_SPECIFIED))
	      {
		if (decl.isPrivate())
		  {
		    if (decl.getFlag(Declaration.PRIVATE_SPECIFIED))
		      error('e', decl,
			    "'", "' is declared both private and exported");
		    decl.setPrivate(false);
		  }
	      }
	    else
	      decl.setPrivate(true);
	  }
	if (moduleStatic)
	  decl.setFlag(Declaration.STATIC_SPECIFIED);
	else if ((mexp.getFlag(ModuleExp.NONSTATIC_SPECIFIED)
		  && ! decl.getFlag(Declaration.STATIC_SPECIFIED))
		 || gnu.expr.Compilation.moduleStatic < 0
		 || mexp.getFlag(ModuleExp.SUPERTYPE_SPECIFIED))
	  decl.setFlag(Declaration.NONSTATIC_SPECIFIED);
      }
    if (! moduleStatic)
      mexp.declareThis(null);

    module = mexp;
    int nforms = forms.size();
    int ndecls = mexp.countDecls();
    pushDecls(mexp);
    mexp.body = makeBody(forms);
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
  private void pop (Declaration decl)
  {
    String sym = decl.getName();
    if (sym == null)
      return;
    popBinding();
  }

  public final void pushDecls (ScopeExp scope)
  {
    // shadowStack.push(null);
    for (Declaration decl = scope.firstDecl();
         decl != null;  decl = decl.nextDecl())
      push(decl);
  }

  private final void popDecls (ScopeExp scope)
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

  /** Note a new binding, remembering old binding in the shadowStack. */
  public void pushBinding(String name, Object value)
  {
    Object old = environ.put(name, value);
    // It is possible the same Declaration may be pushed twice:  Once
    // in scanForDefinitions and once by pushDecls.  With some care
    // this could probably be avoided.  FIXME.
    if (value == old)
      return;
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
}
