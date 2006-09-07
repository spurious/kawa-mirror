package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.text.SourceMessages;
import gnu.lists.*;
import gnu.kawa.lispexpr.*;
import java.util.*;
import gnu.kawa.functions.GetNamedPart;

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

  public Declaration templateScopeDecl;

  /** A variable to hold the matched values for syntax-case
   * pattern variables. */
  public Declaration matchArray;

  /** A stack of aliases pushed by <code>pushRenamedAlias</code>. */
  Stack renamedAliasStack;

  public Stack formStack = new Stack();
  public int firstForm;
  public Object pendingForm;

  public LambdaExp curMethodLambda;

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

  public Translator (Language language, SourceMessages messages)
  {
    super(language, messages);
    this.env = Environment.getCurrent();
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

  /**  The module instance containing the current macro.
   * This is only used temporarily, set when resolving a Declaration
   * bound to a macro, and used to set the macroContext field of the
   * TemplateScope created when expanding the macro's template(s). */
  Declaration macroContext;

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

  final boolean selfEvaluatingSymbol (Object obj)
  {
    return ((LispLanguage) getLanguage()).selfEvaluatingSymbol(obj);
  }

  /** True iff a form matches a literal symbol. */
  public final boolean matches(Object form, String literal)
  {
    return matches(form, null, literal);
  }

  public boolean matches(Object form, SyntaxForm syntax, String literal)
  {
    if (syntax != null)
      {
        // FIXME
      }
    if (form instanceof SyntaxForm)
      {
	// FIXME
	return literal == ((SyntaxForm) form).form;
      }
    if (form instanceof Symbol && ! selfEvaluatingSymbol(form))
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
	&& (getLanguage().getNamespaceOf(decl) & namespace) != 0)
      return decl;
    return currentModule().lookup(name, getLanguage(), namespace);
  }

  /** Find global Declaration, creating one if not found. */
  public Declaration lookupGlobal(Object name)
  {
    return lookupGlobal(name, -1);
  }

  /** Find global Declaration, creating one if not found. */
  public Declaration lookupGlobal(Object name, int namespace)
  {
    ModuleExp module = currentModule();
    Declaration decl = module.lookup(name, getLanguage(), namespace);
    if (decl == null)
      {
        decl = module.getNoDefine(name);
        decl.setIndirectBinding(true);
      }
    return decl;
  }

  /** Check if a Declaration is bound to a Syntax.
   * @param decl the Declaration to check
   * @return the Syntax bound to decl, or null.
   * In the former case, macroContext may be set as a side effect.
   */
  Syntax check_if_Syntax (Declaration decl)
  {
    Declaration d = Declaration.followAliases(decl);

    Expression dval = d.getValue();
    if (dval != null && d.getFlag(Declaration.IS_SYNTAX))
      {
        try
          {
            if (decl.getValue() instanceof ReferenceExp)
              {
                Declaration context
                  = ((ReferenceExp) decl.getValue()).contextDecl();
                if (context != null)
                  macroContext = context;
                else if (current_scope instanceof TemplateScope)
                  macroContext = ((TemplateScope) current_scope).macroContext;
              }
            else if (current_scope instanceof TemplateScope)
              macroContext = ((TemplateScope) current_scope).macroContext;
            Object obj = dval.eval(env);
            return obj instanceof Syntax ? (Syntax) obj : null;
          }
        catch (Throwable ex)
          {
            ex.printStackTrace();
            error('e', "unable to evaluate macro for "+decl.getSymbol());
          }
      }
    return null;
  }

  public Expression rewrite_pair (Pair p, boolean function)
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
	    if (sym instanceof Symbol && ! selfEvaluatingSymbol(sym))
	      {
		symbol = (Symbol) sym;
		name = symbol.getName();
	      }
	    else
	      {
		name = sym.toString();
		symbol = env.getSymbol(name);
	      }
	    proc = env.get(symbol,
			   getLanguage().hasSeparateFunctionNamespace()
			   ? EnvironmentKey.FUNCTION
			   : null,
			   null);
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
            Declaration saveContext = macroContext;
            Syntax syntax = check_if_Syntax (decl);
            if (syntax != null)
              {
                Expression e = apply_rewrite (syntax, p);
                macroContext = saveContext;
                return e;
              }
	  }

	ref.setProcedureName(true);
	if (getLanguage().hasSeparateFunctionNamespace())
	  func.setFlag(ReferenceExp.PREFER_BINDING2);
      }

    int cdr_length = listLength(cdr);

    if (cdr_length == -1)
      return syntaxError("circular list is not allowed after "+p.car);
    if (cdr_length < 0)
      return syntaxError("dotted list ["+cdr+"] is not allowed after "+p.car);

    boolean mapKeywordsToAttributes = false;
    Stack vec = new Stack();

    ScopeExp save_scope = current_scope;
    for (int i = 0; i < cdr_length;)
      {
	if (cdr instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) cdr;
	    cdr = sf.form;
	    setCurrentScope(sf.scope);
	  }
	Pair cdr_pair = (Pair) cdr;
	Expression arg = rewrite_car (cdr_pair, false);
        i++;

        if (mapKeywordsToAttributes)
          {
            if ((i & 1) == 0) // Previous iteration was a keyword
              {
                Expression[] aargs = new Expression[2];
                aargs[0] = (Expression) vec.pop();
                aargs[1] = arg;
                arg = new ApplyExp(gnu.kawa.xml.MakeAttribute.makeAttribute, aargs);
              }
            else
              {
                Object value;
                if (arg instanceof QuoteExp
                    && (value = ((QuoteExp) arg).getValue()) instanceof Keyword
                    && i < cdr_length)
                  arg = new QuoteExp(((Keyword) value).asSymbol());
                else
                  mapKeywordsToAttributes = false;
              }
          }

        vec.addElement(arg);
	cdr = cdr_pair.cdr;
      }
    Expression[] args = new Expression[vec.size()];
    vec.copyInto(args);

    if (save_scope != current_scope)
      setCurrentScope(save_scope);

    return ((LispLanguage) getLanguage()).makeApply(func, args);
  }

  public Symbol namespaceResolve (Expression context, Expression member)
  {
    if (context instanceof ReferenceExp && member instanceof QuoteExp)
      {
        ReferenceExp rexp = (ReferenceExp) context;
        Declaration decl = rexp.getBinding();
        Object val;
        if (decl == null || decl.getFlag(Declaration.IS_UNKNOWN))
          {
            Object rsym = rexp.getSymbol();
            Symbol sym = rsym instanceof Symbol ? (Symbol) rsym
              : env.getSymbol(rsym.toString());
            val = env.get(sym, null);
          }
        else if (decl.isNamespaceDecl())
          {
            val = decl.getConstantValue();
          }
        else
          val = null;
        if (val instanceof Namespace)
          {
            Namespace ns = (Namespace) val;
            String uri = ns.getName();
            if (uri != null && uri.startsWith("class:"))
              return null; 
            String mem = ((QuoteExp) member).getValue().toString().intern();
            return ns.getSymbol(mem);
          }
      }
    return null;
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

  public Object namespaceResolve (Object name)
  {
    if (! (name instanceof String))
      {
        Pair p;
        if (name instanceof Pair
            && safeCar(p = (Pair) name) == LispLanguage.lookup_sym
            && p.cdr instanceof Pair
            && (p = (Pair) p.cdr).cdr instanceof Pair)
          {
            Expression part1 = rewrite(p.car);
            Expression part2 = rewrite(((Pair) p.cdr).car);

            Symbol sym = namespaceResolve(part1, part2);
            if (sym != null)
              return sym;
            String combinedName = GetNamedPart.combineName(part1, part2);
            if (combinedName != null)
              return combinedName;
          }
      }
    return name;
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
      return rewrite_pair((Pair) exp, function);
    else if (exp instanceof String
	     || (exp instanceof Symbol && ! selfEvaluatingSymbol(exp)))
      {
	Declaration decl = lexical.lookup(exp, function);

        // If we're nested inside a class (in a ClassExp) then the field
        // and methods names of this class and super-classes/interfaces
        // need to be searched.
        ScopeExp scope = current_scope;
        int decl_nesting = decl == null ? -1 : ScopeExp.nesting(decl.context);
        String dname;
        if (exp instanceof String
            || (exp instanceof Symbol && ((Symbol) exp).hasEmptyNamespace()))
          dname = exp.toString();
        else
          {
            dname = null;
            scope = null;
          }
        for (;scope != null; scope = scope.outer)
          {
            if (scope instanceof LambdaExp
                && scope.outer instanceof ClassExp // redundant? FIXME
                && ((LambdaExp) scope).isClassMethod())
              {
                if (decl_nesting >= ScopeExp.nesting(scope.outer))
                  break;
                LambdaExp caller = (LambdaExp) scope;
                ClassExp cexp = (ClassExp) scope.outer;
                ClassType ctype = (ClassType) cexp.getType();
                Object part = SlotGet.lookupMember(ctype, dname, ctype);
                boolean contextStatic
                  = (caller == cexp.clinitMethod
                     || (caller != cexp.initMethod 
                         && caller.nameDecl.isStatic()));
                if (part == null)
                  {
                    char mode = contextStatic ? 'S' : 'V';
                    PrimProcedure[] methods
                      = ClassMethods.getMethods(ctype, dname,
                                                mode, ctype, language);
                    if (methods.length == 0)
                      continue;
                  }
                Expression part1;
                // FIXME We're throwing away 'part', which is wasteful.
                if (contextStatic)
                  part1 = new ReferenceExp(((ClassExp) caller.outer).nameDecl);
                else
                  part1 = new ThisExp(caller.firstDecl());
                return GetNamedPart.makeExp(part1,
                                            QuoteExp.getInstance(dname));
              }
          }

	Object nameToLookup;
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
	symbol = exp instanceof String ? env.getSymbol((String) exp)
	  : (Symbol) exp;
	boolean separate = getLanguage().hasSeparateFunctionNamespace();
        if (decl != null)
          {
            if (! isLexical(decl)
                || (separate && decl.isProcedureDecl()))
              decl = null;
          }
        else
          {
	    Location loc
	      = env.lookup(symbol,
			   function && separate ? EnvironmentKey.FUNCTION
			   : null);
	    if (loc != null)
	      loc = loc.getBase();
            if (loc instanceof FieldLocation)
              {
                decl = ((FieldLocation) loc).getDeclaration();
                if (! inlineOk(null)
                    // A kludge - we get a bunch of testsuite failures
                    // if we don't inline $lookup$.  FIXME.
                    && decl != kawa.standard.Scheme.getNamedPartDecl)
                  decl = null;
              }
	    /*
            else if (Compilation.inlineOk && function)
              {
		// Questionable.  fail with new set_b implementation,
		// which just call rewrite_car on the lhs,
		// if we don't require function to be true.  FIXME.
                decl = Declaration.getDeclaration(proc);
              }
            */
          }
	if (decl != null && decl.getContext() instanceof PatternScope)
	  return syntaxError("reference to pattern variable "+decl.getName()+" outside syntax template");

	ReferenceExp rexp = new ReferenceExp (nameToLookup, decl);
	if (current_scope instanceof TemplateScope
            && decl != null && decl.needsContext())
	  rexp.setContextDecl(((TemplateScope) current_scope).macroContext);
        else
          checkMemberContext(rexp, decl);
        rexp.setLine(this);
	if (function && separate)
	  rexp.setFlag(ReferenceExp.PREFER_BINDING2);
	return rexp;
      }
    else if (exp instanceof LangExp)
      return rewrite(((LangExp) exp).getLangValue(), function);
    else if (exp instanceof Expression)
      return (Expression) exp;
    else
      return QuoteExp.getInstance(Quote.quote(exp, this));
  }

  public void checkMemberContext (AccessExp exp, Declaration decl)
  {
    if (decl == null || ! decl.getFlag(Declaration.FIELD_OR_METHOD)
        || decl.isStatic())
      return;
    ScopeExp scope = currentScope();
    for (;;)
      {
        if (scope == null)
          throw new Error("internal error: missing "+decl);
        if (scope.outer == decl.context) // I.e. same class.
          break;
        scope = scope.outer;
      }
    exp.setContextDecl(scope.firstDecl());
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
    if (pair instanceof SyntaxForm)
      pair = ((SyntaxForm) pair).form;
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
    if (exp instanceof QuoteExp)
      return;
    if (exp.getFile () == null)
      exp.setFile(getFile());
    if (exp.getLine () == 0)
      exp.setLine (getLine(), getColumn());
  }

  /** Extract a type from the car of a pair. */
  public Type exp2Type(Pair typeSpecPair)
  {
    Object saved = pushPositionOf(typeSpecPair);
    try
      {
	Expression texp = rewrite_car(typeSpecPair, false);
        texp = new InlineCalls(this).walk(texp);
	if (texp instanceof ErrorExp)
	  return null;
        texp = new InlineCalls(this).walk(texp);
	Type type = getLanguage().getTypeFor(texp);
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
	  result = rewrite_pair(pair, function);  // To avoid a cycle
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
        Declaration saveContext = macroContext;
        Syntax syntax = null;
        ScopeExp save_scope = current_scope;
        try
          {
            Object obj = st_pair.car;
            if (obj instanceof SyntaxForm)
              {
                SyntaxForm sf = (SyntaxForm) st_pair.car;
                setCurrentScope(sf.scope);
                obj = sf.form;
              }
            Pair p;
            if (obj instanceof Pair
                && (p = (Pair) obj).car == LispLanguage.lookup_sym
                && p.cdr instanceof Pair
                && (p = (Pair) p.cdr).cdr instanceof Pair)
              {
                Expression part1 = rewrite(p.car);
                Expression part2 = rewrite(((Pair) p.cdr).car);
                obj = namespaceResolve(part1, part2);
              }
            if (obj instanceof String
                || (obj instanceof Symbol && ! selfEvaluatingSymbol(obj)))
              {
                Declaration decl = lexical.lookup(obj, true);
                if (decl != null)
                  syntax = check_if_Syntax(decl);
                else
                  {
                    obj = resolve(obj, true);
                    if (obj instanceof Syntax)
                      syntax = (Syntax) obj;
                  }
              }
            // Recognize deferred begin created in scanBody for pendingForms.
            // A seemingly-cleaned (obj instanceof Syntax) causes problems
            // with some Syntax forms, such as define.
            else if (obj == kawa.standard.begin.begin)
              syntax = (Syntax) obj;
          }
        finally
          {
            if (save_scope != current_scope)
              setCurrentScope(save_scope);
          }
	if (syntax != null)
	  {
	    String save_filename = getFile();
	    int save_line = getLine();
	    int save_column = getColumn();
	    try
	      {
		setLine(st_pair);
		syntax.scanForm(st_pair, defs, this);
		return;
	      }
	    finally
	      {
                macroContext = saveContext;
		setLine(save_filename, save_line, save_column);
	      }
	  }
      }
    formStack.add(st);
  }

  /** Recursive helper method for rewrite_body.
   * Scan body for definitions, adding partially macro-expanded
   * expressions into the <code>formStack</code>.
   * @param makeList if true, return a list representation of the scanned
   *   forms (not including declarations); else forms are push on formStack
   * @return a list of forms if <code>makeList</code> (possibly wrapped
   * in a <code>SyntaxForm</code>); otherwise <code>null</code>.
   */

  public Object scanBody (Object body, ScopeExp defs, boolean makeList)
  {
    Object list = makeList ? LList.Empty : null;
    Pair lastPair = null;
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
		scanBody(sf.form, defs, false);
		Object f = wrapSyntax(popForms(first), sf);
		if (makeList)
		  {
		    if (lastPair == null)
		      return f;
		    lastPair.cdr = f;
		    return list;
		  }
		formStack.add(f);
		return null;
	      }
	    finally
	      {
		setCurrentScope(save_scope);
	      }
	  }
	else if (body instanceof Pair)
	  {
	    Pair pair = (Pair) body;
	    int first = formStack.size();
	    scanForm(pair.car, defs);
            if (getState() == Compilation.PROLOG_PARSED)
              {
                // We've seen a require form during the initial pass when
                // we're looking module names.  Defer the require and any
                // following forms in this body.
                if (pair.car != pendingForm)
                  pair = makePair(pair, pendingForm, pair.cdr);
                pendingForm = new Pair(kawa.standard.begin.begin, pair);
                return LList.Empty;
              }
	    int fsize = formStack.size();
	    if (makeList)
	      {
		for (int i = first;  i < fsize;  i++)
		  {
		    Pair npair
		      = makePair(pair, formStack.elementAt(i), LList.Empty);
		    if (lastPair == null)
		      list = npair;
		    else
		      lastPair.cdr = npair;
		    lastPair = npair;
		  }
		formStack.setSize(first);
	      }
	    body = pair.cdr;
	  }
	else
	  {
	    formStack.add(syntaxError ("body is not a proper list"));
	    break;
	  }
      }
    return list;
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
	scanBody(exp, defs, false);
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
	  return ((LispLanguage) getLanguage()).makeBody(exps);
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
            decl.getContext().currentLambda().capture(decl);
	    decl.setCanRead(true);
            decl.setSimple(false);
	    decl.setFlag(Declaration.EXTERNAL_ACCESS);
	  }
      }
    if (current_scope != saveScope)
      setCurrentScope(saveScope);
  }

  public void finishModule(ModuleExp mexp)
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
  }

  public void resolveModule(ModuleExp mexp)
  {
    int numPending = pendingImports == null ? 0 : pendingImports.size();
    for (int i = 0;  i < numPending;  )
      {
        ModuleInfo info = (ModuleInfo) pendingImports.elementAt(i++);
        ScopeExp defs = (ScopeExp) pendingImports.elementAt(i++);
        Expression posExp = (Expression) pendingImports.elementAt(i++);
        if (mexp == defs)
          {
            // process(BODY_PARSED);
            Expression savePos = new ReferenceExp((Object) null);
            savePos.setLine(this);
            setLine(posExp);
            kawa.standard.require.importDefinitions(info, null,
                                                    formStack, defs, this);
            setLine(savePos);
            pendingImports.setElementAt(null, i-3);
            pendingImports.setElementAt(null, i-2);
            pendingImports.setElementAt(null, i-1);
          }
      }

    processAccesses();

    setModule(mexp);
    Compilation save_comp = Compilation.getCurrent();
    try
      {
	Compilation.setCurrent(this);
	mexp.body = makeBody(firstForm, mexp);
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
    return makeRenamedAlias(decl.getSymbol(), decl, templateScope);
  }

  public Declaration makeRenamedAlias (Object name,
				       Declaration decl,
				       ScopeExp templateScope)
  {
    Declaration alias = new Declaration(name);
    alias.setAlias(true);
    alias.setPrivate(true);
    alias.context = templateScope;
    ReferenceExp ref = new ReferenceExp(decl);
    ref.setDontDereference(true);
    alias.noteValue(ref);
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

  public Declaration define (Object name, SyntaxForm nameSyntax, ScopeExp defs)
  {
    boolean aliasNeeded = nameSyntax != null && nameSyntax.scope != currentScope();
    Object declName = aliasNeeded ? new String(name.toString()) : name;
    Declaration decl = defs.getDefine(declName, 'w', this);
    if (aliasNeeded)
      {
	Declaration alias = makeRenamedAlias(name, decl, nameSyntax.scope);
	nameSyntax.scope.addDeclaration(alias);
      }
    push(decl);
    return decl;
  }
}
