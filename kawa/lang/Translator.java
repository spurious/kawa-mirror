package kawa.lang;
import kawa.standard.Scheme;
import gnu.bytecode.Method;
import gnu.bytecode.Variable;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.StaticFieldConstraint;
import java.lang.reflect.Modifier;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.text.SourceMessages;
import gnu.lists.*;
import gnu.kawa.lispexpr.LispInterpreter;
import java.util.Hashtable;

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

  /** Set if we're processing a define-syntax or defmacro. */
  public Macro currentMacroDefinition;

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

  public Translator (Environment env, SourceMessages messages)
  {
    super(messages);
    this.env = env;
  }

  public Translator (Environment env)
  {
    super(new SourceMessages());
    this.env = env;
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

  Object resolve(Object name, boolean function)
  {
    Symbol symbol = name instanceof String ? env.lookup((String) name)
      : (Symbol) name;
    if (symbol == null)
      return null;
    if (function && getInterpreter().hasSeparateFunctionNamespace())
      return symbol.getFunctionValue(null);
    if (symbol.isBound())
      return symbol.getValue();
    return null;
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
	    if (decl.getFlag(Declaration.IS_SYNTAX))
	      return apply_rewrite ((Syntax) decl.getConstantValue(), p);
	  }

	ref.setProcedureName(true);
	if (getInterpreter().hasSeparateFunctionNamespace())
	  func.setFlag(ReferenceExp.PREFER_BINDING2);
      }

    int cdr_length = LList.listLength(cdr, false);

    if (cdr_length < 0)
      return syntaxError("dotted list is not allowed");

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
    else if (exp instanceof String || exp instanceof Symbol)
      {
	Declaration decl = lexical.lookup(exp, function);
	Symbol symbol = null;
	if (exp instanceof String && decl == null)
	  {
	    String str = (String) exp;
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
		    decl = (Declaration) uri_decl;
		    Expression dval = decl.getValue();
		    if (dval instanceof QuoteExp)
		      {
			String uri = ((QuoteExp) dval).getValue().toString();
			return rewrite(Symbol.make(uri, local), function);
		      }
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
            else if (Compilation.inlineOk)
              {
                decl = Declaration.getDeclaration(proc);
              }
          }
	if (decl != null && decl.getFlag(Declaration.FIELD_OR_METHOD)
	    && decl.isProcedureDecl() && ! function)
	  return syntaxError("not implemented: variable reference to a method");
	ReferenceExp rexp = new ReferenceExp (nameToLookup, decl);
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
	|| positionPair.getFile() != getFile()
	|| positionPair.getLine() != getLine()
	|| positionPair.getColumn() != getColumn())
      {
	saved = PairWithPosition.make(Special.eof, positionPair,
				      getFile(), getLine(), getColumn());
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
	  result.setFile(getFile());
	if (result.getLine () == 0)
	  result.setLine (getLine(), getColumn());
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
	    String save_filename = getFile();
	    int save_line = getLine();
	    int save_column = getColumn();
	    try
	      {
		setLine(st_pair);
		if (! syntax.scanForDefinitions(st_pair, forms, defs, this))
		  return false;
	      }
	    finally
	      {
		setLine(save_filename, save_line, save_column);
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
    defs.outer = current_scope;
    current_scope = defs;
    try
      {
	if (! scan_body (exp, forms, defs))
	  return new ErrorExp("error while scanning in body");
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
	  }
	Expression body = makeBody(forms, null);
	if (ndecls == 0)
	  return body;
	mustCompileHere();
	defs.body = body;
	return defs;
      }
    finally
      {
	pop(defs);
      }
  }

  /** Combine a list of zero or more expression forms info a "body". */
  public Expression makeBody(java.util.Vector forms, ScopeExp scope)
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
	if (scope instanceof ModuleExp)
	  return new ApplyExp(gnu.kawa.functions.AppendValues.appendValues,
			      exps);
	else
	  return ((LispInterpreter) getInterpreter()).makeBody(exps);
      }
  }

  public void finishModule(ModuleExp mexp, java.util.Vector forms)
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

    setModule(mexp);
    mexp.body = makeBody(forms, mexp);
    lexical.pop(mexp);
    /* DEBUGGING:
    OutPort err = OutPort.errDefault ();
    err.print ("[Re-written expression for load/compile: ");
    mexp.print (err);
    //err.print ("\nbefore load<"+mod.getClass().getName()+">");
    err.println();
    err.flush();
    */
  }
}
