package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.StaticFieldConstraint;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.text.SourceMessages;
import gnu.lists.*;
import gnu.kawa.lispexpr.LispInterpreter;
import java.util.*;

/** Used to translate from source to Expression.
 * The result has macros expanded, lexical names bound, etc, and is
 * ready for code generation.
 * This is sometimes called a "compilation environment",
 * but we modify it as we go along - there is a single Translator for
 * each top-level form.
 */

public class Translator extends Compilation
{
  // Global environment used to look for syntax/macros.
  private Environment env;

  /** Set if we're processing (as opposed to expanding)
   * a <code>define-syntax</code> or <code>defmacro</code>. */
  public Macro currentMacroDefinition;

  /** Innermost current scope of pattern variable,
   * from a <code>syntax-case</code>. */
  public PatternScope patternScope;

  /** A stack of aliases pushed by <code>pushRenamedAlias</code>. */
  Stack renamedAliasStack;

  public Stack formStack = new Stack();

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

  public Translator (Interpreter interp, SourceMessages messages)
  {
    super(interp, messages);
    this.env = interp.getEnvironment();
  }

  public final Environment getGlobalEnvironment() { return env; }

  public Expression parse (Object input)
  {
    return rewrite(input);
  }

  public final Expression rewrite_car (Pair pair, SyntaxForm syntax)
  {
    if (syntax == null || syntax.scope == current_scope
	|| pair.car instanceof SyntaxForm)
      return rewrite_car(pair, false);
    ScopeExp save_scope = current_scope;
    try
      {
	setCurrentScope(syntax.scope);
	return rewrite_car(pair, false);
      }
    finally
      {
	setCurrentScope(save_scope);
      }
  }

  public final Expression rewrite_car (Pair pair, boolean function)
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

  private Object nameToLookup;

  /** Check if declaraton is an alias for some other name.
   * This is needed to chase identifiers renamed for hygienic macro
   * expansion - see SyntaxRules.expand. */
  static ReferenceExp getOriginalRef(Declaration decl)
  {
    if (decl != null && decl.isAlias() && ! decl.isIndirectBinding())
      {
	Expression value = decl.getValue();
	if (value instanceof ReferenceExp)
	  return (ReferenceExp) value;
      }
    return null;
  }

  /** True iff a form matches a literal symbol. */
  public boolean matches(Object form, String literal)
  {
    if (form instanceof SyntaxForm)
      {
	// FIXME
	return literal == ((SyntaxForm) form).form;
      }
    if (form instanceof Symbol)
      {
	ReferenceExp rexp = getOriginalRef(lexical.lookup(form, -1));
	if (rexp != null)
	  form = rexp.getSymbol();
      }
    return form == literal;
  }

  public Declaration lookup(Object name, int namespace)
  {
    Declaration decl = lexical.lookup(name, namespace);
    if (decl != null
	&& (getInterpreter().getNamespaceOf(decl) & namespace) != 0)
      return decl;
    return lookupGlobal(name, namespace);
  }

  public Declaration lookupGlobal(Object name)
  {
    return lookupGlobal(name, -1);
  }

  public Declaration lookupGlobal(Object name, int namespace)
  {
    ModuleExp module = currentModule();
    Declaration decl = module.lookup(name, getInterpreter(), namespace);
    if (decl == null)
      {
        decl = module.getNoDefine(name);
        decl.setIndirectBinding(true);
      }
    return decl;
  }

  /**
   * @param function true if obj is in function-call position (i.e. first).
   */
  Object getBinding (Object obj, boolean function)
  {
    if (obj instanceof String || obj instanceof Symbol)
      {
	Declaration decl = lexical.lookup(obj, function);
	if (decl != null)
	  {
	    ReferenceExp rexp = getOriginalRef(decl);
	    if (rexp != null)
	      {
		decl = rexp.getBinding();
		obj = rexp.getSymbol();
		nameToLookup = obj;
		if (decl == null)
		  {
		    obj = resolve(obj, function);
		  }
		else
		  {
		    obj = decl;
		  }
	      }
	    else
	      {
		nameToLookup = decl.getSymbol();
		obj = decl;
	      }
	  }
	else
	  {
	    nameToLookup = obj;
	    obj = resolve(obj, function);
	  }
        if (obj instanceof Syntax)
          return obj;
	if (obj instanceof Declaration)
	  {
	    Expression dval
	      = Declaration.followAliases((Declaration) obj).getValue();
	    if (dval instanceof QuoteExp)
	      return ((QuoteExp) dval).getValue();
	  }
        if (obj != null)
	  {
            if (obj instanceof Declaration
		     && ! isLexical((Declaration) obj))
	      obj = null;
	  }
	else
	  {
	    Symbol symbol = nameToLookup instanceof Symbol ? (Symbol) nameToLookup
	      : env.lookup(nameToLookup.toString());
	    if (symbol != null && symbol.isBound())
	      return symbol.get();
	  }
	return null;
      }
     return obj;
  }

  /** Check if Object is Syntax, or bound to Syntax.
   * @param obj the value to check
   * @return the Syntax bound to obj, or null.
   */
  Syntax check_if_Syntax (Object obj)
  {
    if (obj instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) obj;
	ScopeExp save_scope = current_scope;
	try
	  {
	    setCurrentScope(sf.scope);
	    obj = getBinding(sf.form, true);
	  }
	finally
	  {
	    setCurrentScope(save_scope);
	  }
      }
    else
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
	    Object sym = ref.getSymbol();
	    Symbol symbol;
	    String name;
	    if (sym instanceof Symbol)
	      {
		symbol = (Symbol) sym;
		name = symbol.getName();
	      }
	    else
	      {
		name = sym.toString();
		symbol = env.lookup(name);
	      }
	    if (symbol != null)
	      if (getInterpreter().hasSeparateFunctionNamespace())
		proc = symbol.getFunctionValue(null);
	      else
		proc = symbol.get(null);
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
        else
	  {
	    decl = Declaration.followAliases(decl);
	    proc = decl.getConstantValue();
	    if (proc instanceof Syntax)
	      return apply_rewrite ((Syntax) proc, p);
	  }

	ref.setProcedureName(true);
	if (getInterpreter().hasSeparateFunctionNamespace())
	  func.setFlag(ReferenceExp.PREFER_BINDING2);
      }

    int cdr_length = listLength(cdr);

    if (cdr_length < 0)
      return syntaxError("dotted list is not allowed");

    Expression[] args = new Expression[cdr_length];

    ScopeExp save_scope = current_scope;
    for (int i = 0; i < cdr_length; i++)
      {
	if (cdr instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) cdr;
	    cdr = sf.form;
	    setCurrentScope(sf.scope);
	  }
	Pair cdr_pair = (Pair) cdr;
	args[i] = rewrite_car (cdr_pair, false);
	cdr = cdr_pair.cdr;
      }
    if (save_scope != current_scope)
      setCurrentScope(save_scope);

    return ((LispInterpreter) getInterpreter()).makeApply(func, args);
  }

  public static Object stripSyntax (Object obj)
  {
    while (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).form;
    return obj;
  }

  public static Object safeCar (Object obj)
  {
    while (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).form;
    if (! (obj instanceof Pair))
      return null;
    return stripSyntax(((Pair) obj).car);
  }

  public static Object safeCdr (Object obj)
  {
    while (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).form;
    if (! (obj instanceof Pair))
      return null;
    return stripSyntax(((Pair) obj).cdr);
  }

  /** Returns the length of a syntax list.
   * Returns Integer.MIN_VALUE for cyclic lists.
   * For impure lists returns the negative of one more than
   * the number of pairs before the "dot".
   * Similar to LList.listLength, but descends into SyntaxForm. */
  public static int listLength(Object obj)
  {
    // Based on list-length implementation in
    // Guy L Steele jr: "Common Lisp:  The Language", 2nd edition, page 414
    int n = 0;
    Object slow = obj;
    Object fast = obj;
    for (;;)
      {
	// 'n' is number of previous Pairs before 'fast' cursor.
	while (fast instanceof SyntaxForm)
	  fast = ((SyntaxForm) fast).form;
	while (slow instanceof SyntaxForm)
	  slow = ((SyntaxForm) slow).form;
	if (fast == LList.Empty)
	  return n;
	if (! (fast instanceof Pair))
	  return -1-n;
	n++;
	Object next = ((Pair) fast).cdr;
	while (next instanceof SyntaxForm)
	  next = ((SyntaxForm) next).form;
	if (next == LList.Empty)
	  return n;
	if (! (next instanceof Pair))
	  return -1-n;
	slow = ((Pair)slow).cdr;
	fast = ((Pair)next).cdr;
	n++;
	if (fast == slow)
	  return Integer.MIN_VALUE;
      }
  }

  public void rewriteInBody (Object exp)
  {
    if (exp instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) exp;
	ScopeExp save_scope = current_scope;
	try
	  {
	    setCurrentScope(sf.scope);
	    rewriteInBody(sf.form);
	  }
	finally
	  {
	    setCurrentScope(save_scope);
	  }
      }
    else if (exp instanceof Values)
      {
	Object[] vals = ((Values) exp).getValues();
	for (int i = 0;  i < vals.length;  i++)
	  rewriteInBody(vals[i]);
      }
    else
      formStack.add(rewrite(exp, false));
  }

  /**
   * Re-write a Scheme expression in S-expression format into internal form.
   */
  public Expression rewrite (Object exp)
  {
    return rewrite(exp, false);
  }

  public void setCurrentScope (ScopeExp scope)
  {
    super.setCurrentScope(scope);
    while (scope != null && ! (scope instanceof PatternScope))
      scope = scope.outer;
    patternScope = (PatternScope) scope;
  }

  /**
   * Re-write a Scheme expression in S-expression format into internal form.
   */
  public Expression rewrite (Object exp, boolean function)
  {
    if (exp instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) exp;
	ScopeExp save_scope = current_scope;
	try
	  {
	    setCurrentScope(sf.scope);
	    Expression s = rewrite(sf.form, function);
	    return s;
	  }
	finally
	  {
	    setCurrentScope(save_scope);
	  }
      }
    if (exp instanceof PairWithPosition)
      return rewrite_with_position (exp, function, (PairWithPosition) exp);
    else if (exp instanceof Pair)
      return rewrite_pair ((Pair) exp);
    else if (exp instanceof String || exp instanceof Symbol)
      {
	Declaration decl = lexical.lookup(exp, function);
	Symbol symbol = null;
	if (decl != null)
	  {
	    nameToLookup = decl.getSymbol();
	    exp = null;
	    ReferenceExp rexp = getOriginalRef(decl);
	    if (rexp != null)
	      {
		decl = rexp.getBinding();
		if (decl == null)
		  {
		    exp = rexp.getSymbol();
		    nameToLookup = exp;
		  }
	      }
	  }
	else
	  {
	    nameToLookup = exp;
	  }
	if (nameToLookup instanceof String && decl == null)
	  {
	    String str = (String) nameToLookup;
	    int colon = str.indexOf(':');
	    if (colon > 0 && colon < str.length() - 1)
	      {
		String prefix = str.substring(0, colon);
		String local = str.substring(colon + 1);
		String xprefix
		  = (Interpreter.NAMESPACE_PREFIX+prefix).intern();
		Object uri_decl = lexical.lookup(xprefix, function);
		if (uri_decl instanceof Declaration)
		  {
		    decl = Declaration.followAliases((Declaration) uri_decl);
		    Expression dval = decl.getValue();
		    if (dval instanceof QuoteExp)
		      {
			String uri = ((QuoteExp) dval).getValue().toString();
			return rewrite(Symbol.make(uri, local), function);
		      }
		    decl = null;
		  }
		else
		  {
		    Object v = resolve(env.lookup(xprefix), function);
		    if (v != null)
		      return rewrite(Symbol.make(v.toString(), local),
				     function);
		    try
		      {
			Class cl = Class.forName(prefix);
			return rewrite(Symbol.make("class:"+prefix, local),
				       function);
		      }
		    catch (Exception ex)
		      {
		      }
		  }
	      }
	  }
	symbol = exp instanceof String ? env.lookup((String) exp)
	  : (Symbol) exp;
	Object value = resolve(symbol, function);
	boolean separate = getInterpreter().hasSeparateFunctionNamespace();
        if (decl != null)
          {
            if (! isLexical(decl)
                || (separate && decl.isProcedureDecl()))
              decl = null;
          }
        else if (value instanceof Named)
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
            Constraint constraint = symbol.getConstraint();
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
            else if (Compilation.inlineOk && function)
              {
		// Questionable.  fail with new set_b implementation,
		// which just call rewrite_car on the lhs,
		// if we don't require function to be true.  FIXME.
                decl = Declaration.getDeclaration(proc);
              }
          }
	if (decl != null && decl.getFlag(Declaration.FIELD_OR_METHOD)
	    && decl.isProcedureDecl() && ! function)
	  return syntaxError("not implemented: variable reference to a method");
	if (decl != null && decl.getContext() instanceof PatternScope)
	  return syntaxError("reference to pattern variable "+decl.getName()+" outside syntax template");
	  
	ReferenceExp rexp = new ReferenceExp (nameToLookup, decl);
	if (separate)
	  rexp.setFlag(ReferenceExp.PREFER_BINDING2);
	return rexp;
      }
    else if (exp instanceof LangExp)
      return rewrite(((LangExp) exp).getLangValue(), function);
    else if (exp instanceof Expression)
      return (Expression) exp;
    else
      return QuoteExp.getInstance(exp);
  }

  public static void setLine(Expression exp, Object pair)
  {
    if (pair instanceof PairWithPosition)
      {
	PairWithPosition expPos = (PairWithPosition) pair;
	exp.setFile(expPos.getFile());
	exp.setLine(expPos.getLine(), expPos.getColumn());
      }
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
    Object saved;
    if (positionPair == null
	|| positionPair.getFile() != getFile()
	|| positionPair.getLine() != getLine()
	|| positionPair.getColumn() != getColumn())
      {
	saved = PairWithPosition.make(Special.eof, positionPair,
				      getFile(), getLine(), getColumn());
      }
    else
      saved = positionPair;
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

  /** Set the line position of the argument to the current position. */

  public void setLineOf (Expression exp)
  {
    if (exp.getFile () == null)
      exp.setFile(getFile());
    if (exp.getLine () == 0)
      exp.setLine (getLine(), getColumn());
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
	     return Type.pointer_type;
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
	setLineOf(result);
      }
    finally
      {
	popPositionOf(saved);
      }
    return result;
  }

  public static Object wrapSyntax (Object form, SyntaxForm syntax)
  {
    if (syntax == null || form instanceof Expression)
      return form;
    else
      return syntax.fromDatumIfNeeded(form);
  }

  public Object popForms (int first)
  {
    int last = formStack.size();
    if (last == first)
      return Values.empty;
    Object r;
    if (last == first + 1)
      r = formStack.elementAt(first);
    else
      {
	Values vals = new Values();
	for (int i = first; i < last;  i++)
	  vals.writeObject(formStack.elementAt(i));
	r = vals;
      }
    formStack.setSize(first);
    return r;
  }

  public void scanForm (Object st, ScopeExp defs)
  {
    if (st instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) st;
	ScopeExp save_scope = currentScope();
	try
	  {
	    setCurrentScope(sf.scope);
	    int first = formStack.size();
	    scanForm(sf.form, defs);
	    formStack.add(wrapSyntax(popForms(first), sf));
	    return;
	  }
	finally
	  {
	    setCurrentScope(save_scope);
	  }
      }
    if (st instanceof Values)
      {
	if (st == Values.empty)
	  st = QuoteExp.voidExp; // From #!void
	else
	  {
	    Object[] vals = ((Values) st).getValues();
	    for (int i = 0;  i < vals.length;  i++)
	      scanForm(vals[i], defs);
	    return;
	  }
      }
    if (st instanceof Pair)
      {
        Pair st_pair = (Pair) st;
        Syntax syntax = check_if_Syntax(st_pair.car);
	if (syntax != null)
	  {
	    String save_filename = getFile();
	    int save_line = getLine();
	    int save_column = getColumn();
	    Compilation save_comp = Compilation.getCurrent();
	    try
	      {
		Compilation.setCurrent(this);
		setLine(st_pair);
		syntax.scanForm(st_pair, defs, this);
		return;
	      }
	    finally
	      {
		Compilation.setCurrent(save_comp);
		setLine(save_filename, save_line, save_column);
	      }
	  }
      }
    formStack.add(st);
  }

  /** Recursive helper method for rewrite_body.
   * Scan body for definitions, placing partially macro-expanded
   * expressions into forms.
   * If definitions were seen, return a LetExp containing the definitions.
   */

  public void scanBody (Object body, ScopeExp defs)
  {
    while (body != LList.Empty)
      {
	if (body instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) body;
	    ScopeExp save_scope = current_scope;
	    try
	      {
		setCurrentScope(sf.scope);
		int first = formStack.size();
		scanBody(sf.form, defs);
		formStack.add(wrapSyntax(popForms(first), sf));
		return;
	      }
	    finally
	      {
		setCurrentScope(save_scope);
	      }
	  }
	else if (body instanceof Pair)
	  {
	    Pair pair = (Pair) body;
	    scanForm(pair.car, defs);
	    body = pair.cdr;
	  }
	else
	  {
	    formStack.add(syntaxError ("body is not a proper list"));
	    break;
	  }
      }
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
    // NOTE we have both a rewrite_body and a rewriteBody.
    // This is confusing, at the least.  FIXME.
    Object saved = pushPositionOf(exp);
    LetExp defs = new LetExp(null);
    int first = formStack.size();
    defs.outer = current_scope;
    current_scope = defs;
    try
      {
	scanBody(exp, defs);
	if (formStack.size() == first)
	  formStack.add(syntaxError ("body with no expressions"));
	int ndecls = defs.countDecls();
	if (ndecls != 0)
	  {
	    Expression[] inits = new Expression[ndecls];
	    for (int i = ndecls;  --i >= 0; )
	      inits[i] = QuoteExp.undefined_exp;
	    defs.inits = inits;
	  }
	Expression body = makeBody(first, null);
	setLineOf(body);
	if (ndecls == 0)
	  return body;
	mustCompileHere();
	defs.body = body;
	setLineOf(defs);
	return defs;
      }
    finally
      {
	pop(defs);
	popPositionOf(saved);
      }
  }

  /* Rewrite forms on formStack above first. */
  public void rewriteBody (int first)
  {
    int nforms = formStack.size() - first;
    if (nforms == 0)
      return;
    else if (nforms == 1)
      {
	Object f = formStack.pop();
	rewriteInBody(f);
      }
    else
      {
	Object[] forms = new Object [nforms];
	for (int i = 0; i < nforms; i++)
	  forms[i] = formStack.elementAt(first + i);
	formStack.setSize(first);
	for (int i = 0; i < nforms; i++)
	  rewriteInBody(forms[i]);
      }
  }

  /** Combine a list of zero or more expression forms into a "body". */
  public Expression makeBody(int first, ScopeExp scope)
  {
    rewriteBody(first);
    int nforms = formStack.size() - first;
    if (nforms == 0)
      return QuoteExp.voidExp; 
    else if (nforms == 1)
      {
	return (Expression) formStack.pop();
      }
    else
      {
	Expression[] exps = new Expression[nforms];
	for (int i = 0; i < nforms; i++)
	  exps[i] = (Expression) formStack.elementAt(first + i);
	formStack.setSize(first);
	if (scope instanceof ModuleExp)
	  return new ApplyExp(gnu.kawa.functions.AppendValues.appendValues,
			      exps);
	else
	  return ((LispInterpreter) getInterpreter()).makeBody(exps);
      }
  }

  /** Storage used by noteAccess and processAccesses. */
  Vector notedAccess;

  /** Note that we reference name in a given scope.
   * This may be called when defining a macro, at scan-time,
   * and the name may be bound to a declaration we haven't seen yet. */
  public void noteAccess (Object name, ScopeExp scope)
  {
    if (notedAccess == null)
      notedAccess = new Vector();
    notedAccess.addElement(name);
    notedAccess.addElement(scope);
  }

  /** Check references recorded by noteAccess.
   * Resolve now to a Declaration, and note the access.
   * This is needed in case an exported macro references a private Declaration.
   */
  public void processAccesses ()
  {
    if (notedAccess == null)
      return;
    int sz = notedAccess.size();
    ScopeExp saveScope = current_scope;
    for (int i = 0;  i < sz;  i += 2)
      {
	Object name = notedAccess.elementAt(i);
	ScopeExp scope = (ScopeExp) notedAccess.elementAt(i+1);
	if (current_scope != scope)
	  setCurrentScope(scope);
	Declaration decl =  (Declaration) lexical.lookup(name, -1);
	if (decl != null && ! decl.getFlag(Declaration.IS_UNKNOWN))
	  {
	    decl.setCanRead(true);
	    decl.setFlag(Declaration.EXTERNAL_ACCESS);
	  }
      }
    if (current_scope != saveScope)
      setCurrentScope(saveScope);
  }

  public void finishModule(ModuleExp mexp, int first)
  {
    boolean moduleStatic = mexp.isStatic();
    for (Declaration decl = mexp.firstDecl();
	 decl != null;  decl = decl.nextDecl())
      {
	if (decl.getFlag(Declaration.NOT_DEFINING)
	    && ! decl.getFlag(Declaration.IS_UNKNOWN))
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

    processAccesses();

    setModule(mexp);
    Compilation save_comp = Compilation.getCurrent();
    try
      {
	Compilation.setCurrent(this);
	mexp.body = makeBody(first, mexp);
	lexical.pop(mexp);
      }
    finally
      {
	Compilation.setCurrent(save_comp);
      }

    /* DEBUGGING:
    OutPort err = OutPort.errDefault ();
    err.print ("[Re-written expression for load/compile: ");
    mexp.print (err);
    //err.print ("\nbefore load<"+mod.getClass().getName()+">");
    err.println();
    err.flush();
    */
  }

  public Declaration makeRenamedAlias (Declaration decl,
				       ScopeExp templateScope)
  {
    if (templateScope == null)
      return decl; // ???
    Object name = decl.getSymbol();
    Declaration alias = new Declaration(name);
    alias.setAlias(true);
    alias.noteValue(new ReferenceExp(decl));
    alias.setPrivate(true);
    alias.context = templateScope;
    return alias;
  }

  /** Push an alias for a declaration in a scope.
   * If the name of <code>decl</code> came from a syntax template
   * whose immediate scope is <code>templateScope</code>,
   * then the same syntax template may contain local variable references
   * that are also in the same <code>templateScope</code>.
   * Such variable references will <em>not</em> look in the current
   * "physical" scope, where we just created <code>decl</code>, but
   * will instead search the "lexical" <code>templateScope</scope>.
   * So that such references can resolve to <code>decl</code>, we
   * create an alias in <code>templateScope</code> that points
   * to <code>decl</code>.  We record that we did this in the
   * <code> renamedLiasStack</code>, so we can remove the alias later.
   */
  public void pushRenamedAlias (Declaration alias)
  {
    Declaration decl = getOriginalRef(alias).getBinding();
    ScopeExp templateScope = alias.context;
    decl.setSymbol(null);
    Declaration old = templateScope.lookup(decl.getSymbol());
    if (old != null)
      templateScope.remove(old);
    templateScope.addDeclaration(alias);
    if (renamedAliasStack == null)
      renamedAliasStack = new Stack();
    renamedAliasStack.push(old);
    renamedAliasStack.push(alias);
    renamedAliasStack.push(templateScope);
  }

  /** Remove one or more aliases created by <code>pushRenamedAlias</code>. */
  public void popRenamedAlias (int count)
  {
    while (--count >= 0)
      {
	ScopeExp templateScope = (ScopeExp) renamedAliasStack.pop();
	Declaration alias = (Declaration) renamedAliasStack.pop();
	Declaration decl = getOriginalRef(alias).getBinding();
	decl.setSymbol(alias.getSymbol());
	templateScope.remove(alias);
	Object old = renamedAliasStack.pop();
	if (old != null)
	  templateScope.addDeclaration((Declaration) old);
      }
  }
}
