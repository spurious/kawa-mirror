// Copyright (c) 2003, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import java.util.Hashtable;
import java.io.Externalizable;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.text.SourceLocator;

public class FindCapturedVars extends ExpExpVisitor<Void>
{
  public static void findCapturedVars (Expression exp, Compilation comp)
  {
    FindCapturedVars visitor = new FindCapturedVars();
    visitor.setContext(comp);
    exp.visit(visitor, null);
  }

  int backJumpPossible = 0;

  protected final void visitDeclarationType (Declaration decl)
  {
    // If decl.typeExp references a ClassExp then we might get a
    // needless capture of the ClassExp's declarations.
    // For now ignore the issue ... FIXME
  }

  protected Expression visitApplyExp (ApplyExp exp, Void ignored)
  {
    int oldBackJumpPossible = backJumpPossible;
    boolean skipFunc = false;
    boolean skipArgs = false;
    // If the func is bound to a module-level known function, and it
    // doesn't need a closure yet (i.e. could be compiled to a static
    // method), don't visit the function, since that might force it to
    // unnecessarily get "captured" which might force the current
    // function to require a closure.  That would be wasteful if the
    // alternative is to just call func using invokestatic.  (It is
    // possible that we later find out that func needs a static link,
    // in which case the current function does as well;  this is taken
    // care of by calling setCallersNeedStaticLink in LambdaExp.)
    // (This code may be less useful now that --module-static is the default.)
    if (exp.func instanceof ReferenceExp
	&& getCompilation().currentCallConvention() <= Compilation.CALL_WITH_RETURN)
      {
	Declaration decl
	  = Declaration.followAliases(((ReferenceExp) exp.func).binding);
	if (decl != null && decl.context instanceof ModuleExp
            && ! decl.isPublic()
            && ! decl.getFlag(Declaration.NONSTATIC_SPECIFIED))
	  {
	    Expression value = decl.getValue();
	    if (value instanceof LambdaExp)
	      {
		LambdaExp lexp = (LambdaExp) value;
		if (! lexp.getNeedsClosureEnv())
                  skipFunc = true;
	      }
	  }
      }
    // Similar hack for constructor calls, but here we want to
    // avoid visiting the type argument.
    else if (exp.func instanceof QuoteExp && exp.getArgCount() > 0)
      {
        Object val = ((QuoteExp) exp.func).getValue();
        Expression arg0 = exp.getArg(0);
        if (val instanceof PrimProcedure
            && ((PrimProcedure) val).isConstructor()
            && arg0 instanceof ReferenceExp)
          {
            Declaration decl
              = Declaration.followAliases(((ReferenceExp) arg0).binding);
            if (decl != null && decl.context == comp.getModule()
                && ! decl.getFlag(Declaration.NONSTATIC_SPECIFIED))
              {
                Expression value = decl.getValue();
                if (value instanceof ClassExp)
                  {
                    Expression[] args = exp.getArgs();
                    LambdaExp lexp = (LambdaExp) value;
                    if (! lexp.getNeedsClosureEnv())
                      {
                        decl.addCaller(exp);
                        for (int i = 1;  i < args.length;  i++)
                          args[i].visit(this, ignored);
                        skipFunc = skipArgs = true;
                      }
                  }
              }
          }
      }
    if (! skipFunc)
      exp.func = exp.func.visit(this, ignored);
    if (exitValue == null && ! skipArgs) {
        int nargs = exp.args.length;
        for (int i = 0; i < nargs;  i++) {
            Expression arg = visit(exp.args[i], null);
            // It is possible that visiting args[i] may cause prefix
            // arguments to be inserted (search for IS_CAPTURED)
            // so we need to adjust for that.
            int inserted = exp.args.length - nargs;
            i += inserted;
            exp.args[i] = arg;
            nargs += inserted;
        }
    }
    if (backJumpPossible > oldBackJumpPossible)
      exp.setFlag(ApplyExp.MAY_CONTAIN_BACK_JUMP);
    return exp;
  }

  public void visitDefaultArgs (LambdaExp exp, Void ignored)
  {
    super.visitDefaultArgs(exp, ignored);

    // Check if any default expression "captured" a parameter.
    // If so, evaluating a default expression cannot be done until the
    // heapFrame is allocated in the main-method.  But in most cases, a
    // default expression will not contain a nested scope, hence no
    // capture, hence we can generate efficient code to handle optional
    // arguments.
    for (Declaration param = exp.firstDecl();
	 param != null; param = param.nextDecl())
      {
	if (! param.isSimple())
	  {
	    exp.setFlag(true, LambdaExp.DEFAULT_CAPTURES_ARG);
	    break;
	  }
      }
  }

  protected Expression visitClassExp (ClassExp exp, Void ignored)
  {
    Expression ret = super.visitClassExp(exp, ignored);
    if (! exp.explicitInit && ! exp.instanceType.isInterface())
      // Make sure <init> has been declared, in case we need to invoke it.
      Compilation.getConstructor(exp.instanceType, exp);
    else if (exp.getNeedsClosureEnv())
      {
        for (LambdaExp child = exp.firstChild;  child != null;
             child = child.nextSibling)
          {
            if ("*init*".equals(child.getName()))
              child.setNeedsStaticLink(true);
          }
      }
    if (exp.isSimple() && exp.getNeedsClosureEnv() && exp.nameDecl != null
        && exp.nameDecl.getType() == Compilation.typeClass)
      exp.nameDecl.setType(Compilation.typeClassType);
    return ret;
  }

  protected Expression visitModuleExp (ModuleExp exp, Void ignored)
  {
    ModuleExp saveModule = currentModule;
    Hashtable saveDecls = unknownDecls;
    currentModule = exp;
    unknownDecls = null;
    try
      {
	return visitLambdaExp(exp, ignored);
      }
    finally
      {
	currentModule = saveModule;
	unknownDecls = saveDecls;
      }
  }

  void maybeWarnNoDeclarationSeen(Object name, boolean function,
                                  Compilation comp, SourceLocator location)
  {
    if (comp.resolve(name, function) == null)
      maybeWarnNoDeclarationSeen(name, comp, location);
  }

  void maybeWarnNoDeclarationSeen (Object name, Compilation comp, SourceLocator location)
  {
    if (comp.warnUndefinedVariable())
      comp.error('w', "no declaration seen for "+name, location);
  }

  protected Expression visitFluidLetExp (FluidLetExp exp, Void ignored)
  {
    for (Declaration decl = exp.firstDecl(); decl != null; decl = decl.nextDecl())
      {
        if (decl.base == null)
          {
            Object name = decl.getSymbol();
            Declaration bind = allocUnboundDecl(name, false);
            if (! decl.getFlag(Declaration.IS_DYNAMIC))
              maybeWarnNoDeclarationSeen(name, comp, exp);
            capture(bind, null);
            decl.base = bind;
          }
      }
    return super.visitLetExp(exp, ignored);
  }

  protected Expression visitLetExp (LetExp exp, Void ignored)
  {
    if (exp.body instanceof BeginExp)
      {
	// Optimize "letrec"-like forms.
	// If init[i] is the magic QuoteExp.nullExp, and the real value
	// is a LambdaExp or a QuoteExp, we're not going to get weird
	// order-dependencies, and it is safe to transform it to a regular let.
	// It's also necessary in the case of a LambdaExp if it shares
	// a field with the declaration (see LambdaExp.allocFieldField),
	// since assigning the nullExp can clobber the field after it has
	// been initialized with a ModuleMethod.
	Expression[] exps = ((BeginExp) exp.body).exps;
	int init_index = 0;
	Declaration decl = exp.firstDecl();
	for (int begin_index = 0;
	     begin_index < exps.length && decl != null;
	     begin_index++)
	  {
	    Expression st = exps[begin_index];
	    if (st instanceof SetExp)
	      {
		SetExp set = (SetExp) st;
		if (set.binding == decl
		    && decl.getInitValue() == QuoteExp.nullExp
		    && set.isDefining())
		  {
		    Expression new_value = set.new_value;
		    if ((new_value instanceof QuoteExp
			 || new_value instanceof LambdaExp)
			&& decl.getValue() == new_value)
		      {
			decl.setInitValue(new_value);
			exps[begin_index] = QuoteExp.voidExp;
		      }
		    init_index++;
		    decl = decl.nextDecl();
		  }
	      }
	  }
      }
    return super.visitLetExp(exp, ignored);
  }

  protected Expression visitLambdaExp (LambdaExp exp, Void ignored)
  {
    if (exp.getInlineOnly())
        backJumpPossible++;
    return super.visitLambdaExp(exp, ignored);
  }

    protected Expression visitCaseExp(CaseExp exp, Void ignored) {

        exp.key = visit(exp.key, ignored);
        for (int i = 0; i < exp.clauses.length; i++) {
            Expression e = exp.clauses[i].exp;
            e = visit(e, ignored);
        }

        CaseExp.CaseClause ecl = exp.elseClause;
        if (ecl != null)
            ecl.exp = visit(ecl.exp, ignored);

        return exp;
    }

  public void capture(Declaration decl, ReferenceExp rexp)
  {
    if (! decl.getCanReadOrCall())
      return;
    if (decl.field != null && decl.field.getStaticFlag())
      return;
    // This catches the "(module-instance)" dummy context variable
    // created in Translator.rewrite.
    if (comp.immediate && decl.hasConstantValue())
      return;

    LambdaExp curLambda = getCurrentLambda ();
    ScopeExp sc = decl.getContext();
    LambdaExp declLambda = sc.currentLambda ();

    // If curLambda is inlined, the function that actually needs a closure
    // is its caller.  We get its caller using getCaller().
    // A complication is that we can have a chain of functions that
    // recursively call each other, and are hence inlined in each other.
    // Since a function is only inlined if it has a single call site,
    // that means there is actually no way to actually enter the chain;
    // i.e. none of the inlined functions can actually get called.
    // However, we have to watch out for this possibility, or the loop
    // here will run forever.  For us to have a cycle, all of the functions
    // must have the same parent.  If the loop is executed more times
    // than the number of child functions of the parent, then we know we
    // have a cycle.
    // The `chain' variable is used to catch infinite inline loops by
    // iterating through the parents children.
    LambdaExp oldParent = null;
    LambdaExp chain = null;
    while (curLambda != declLambda && curLambda.getInlineOnly())
      {
        LambdaExp curParent = curLambda.outerLambda();
        if (curParent != oldParent)
          {
            // Reset the chain.
            chain = curParent.firstChild;
            oldParent = curParent;
          }
        if (chain == null || curLambda.inlineHome == null)
          {
            // Infinite loop of functions that are inlined in each other.
            return;
          }
        curLambda = curLambda.getCaller();
        chain = chain.nextSibling;
      }
    if (comp.usingCPStyle())
      {
	if (curLambda instanceof ModuleExp)
	  return;
      }
    else
      if (curLambda == declLambda)
	return;

    // The logic here is similar to that of decl.ignorable():
    Expression value = decl.getValue();
    LambdaExp declValue;
    if (value == null || ! (value instanceof LambdaExp))
      declValue = null;
    else
      {
        declValue = (LambdaExp) value;
        if (declValue.getInlineOnly())
          return;
        if (declValue.isHandlingTailCalls())
          declValue = null;
	else if (declValue == curLambda && ! decl.getCanRead())
          return;
      }

    if (decl.getFlag(Declaration.IS_UNKNOWN))
      {
	// Don't create a closure for a static function/class.
	for (LambdaExp parent = curLambda; ; parent = parent.outerLambda())
	  {
	    if (parent == declLambda)
	      break;
	    if (parent.nameDecl != null
		&& parent.nameDecl.getFlag(Declaration.STATIC_SPECIFIED))
	      {
		decl.setFlag(Declaration.STATIC_SPECIFIED);
		break;
	      }
	  }
      }
    if (decl.base != null)
      {
	decl.base.setCanRead(true);
	capture(decl.base, null);
      }
    else if (decl.getCanReadOrCall() || declValue == null)
      {
	if (! decl.isStatic())
	  {
	    LambdaExp heapLambda = curLambda;

            // Perform the "lambda lifting" optimization, if possible:
            //   (define (f x) ... y ...)
            //   (f a)
            // to:
            //   (define (f y$ x) ... y$ ...)
            //   (f y a)
            // This can avoid the need for creating a closure.
            if (rexp != null
                && decl.nvalues == 1 && ! decl.hasUnknownValue()
                && ! (decl.getValueRaw() instanceof LambdaExp)
                // don't confuse call/cc inlining (over-conservative )
                && ! decl.getFlag(Declaration.DONT_COPY)
                && ! curLambda.getInlineOnly() // FIXME - for simplicity
                && ! curLambda.getCanRead() && curLambda.nameDecl != null
                && ! curLambda.nameDecl.context.isClassGenerated()
                && curLambda.min_args == curLambda.max_args) {
                Declaration ndecl = null;
                for (ndecl = curLambda.firstDecl();  ndecl != null;
                     ndecl = ndecl.nextDecl()) {
                    if (ndecl.getFlag(Declaration.IS_CAPTURED)
                        && ndecl.base == decl)
                        break;
                }
                if (ndecl == null) {
                    ndecl = new Declaration(decl.getSymbol());
                    ndecl.base = decl;
                    ndecl.setFlag(Declaration.IS_CAPTURED);
                    ndecl.setCanRead(true);
                    curLambda.add(null, ndecl);
                    curLambda.min_args++;
                    curLambda.max_args++;
                    for (ApplyExp exp = curLambda.nameDecl.firstCall;
                         exp != null;  exp = exp.nextCall) {
                        LambdaExp context = exp.context;
                        Expression[] args = exp.getArgs();
                        Expression[] nargs = new Expression[args.length+1];
                        boolean recursive = exp.context == curLambda;
                        ReferenceExp ref =
                            new ReferenceExp(recursive ? ndecl : decl);
                        nargs[0] = ref;
                        System.arraycopy(args, 0, nargs, 1, args.length);
                        exp.setArgs(nargs);
                        LambdaExp saveLambda = currentLambda;
                        currentLambda = context;
                        capture(decl, ref);
                        currentLambda = saveLambda;
                    }
                }
                rexp.setBinding(ndecl);
                return;
            }
 
            if (decl.context instanceof ClassExp) {
                if (heapLambda.getOuter() == decl.context)
                    return;
                ScopeExp methodLambda = heapLambda;
                while (methodLambda != null) {
                    ScopeExp outer = methodLambda.getOuter();
                    if (outer == decl.context) {
                        Declaration thisDecl = methodLambda.firstDecl();
                        if (thisDecl != null && thisDecl.isThisParameter()) {
                            capture(thisDecl, null);
                            return;
                        }
                        break;
                    }
                    methodLambda = outer;
                }
            }
            if (! decl.isFluid()) {
                heapLambda.setImportsLexVars();
            }
	    LambdaExp parent = heapLambda.outerLambda();
	    for (LambdaExp outer = parent;  outer != declLambda && outer != null; )
	      {
		heapLambda = outer;
		if (! decl.getCanReadOrCall() && declValue == outer)
		  break;
		Declaration heapDecl = heapLambda.nameDecl;
		if (heapDecl != null
		    && heapDecl.getFlag(Declaration.STATIC_SPECIFIED))
		  {
		    comp.error('e', "static " + heapLambda.getName()
			       + " references non-static " + decl.getName());
		  }
                if (heapLambda instanceof ClassExp
                    && heapLambda.getName() != null
                    && ((ClassExp) heapLambda).isSimple())
                  comp.error('w', heapLambda.nameDecl,
                             "simple class ", " requiring lexical link (because of reference to "+decl.getName()+") - use define-class instead");
		heapLambda.setNeedsStaticLink();
		outer = heapLambda.outerLambda();
	      }
	  }
        declLambda.capture(decl);
      }
  }

  Hashtable unknownDecls = null;
  ModuleExp currentModule = null;

  Declaration allocUnboundDecl(Object name, boolean function)
  {
    Declaration decl;
    Object key = name;
    if (function && name instanceof Symbol)
      {
	if (! getCompilation().getLanguage().hasSeparateFunctionNamespace())
	  function = false;
	else // FIXME maybe just use gnu.lists.Pair and remove KeyPair class?
	  key = new KeyPair((Symbol) name, EnvironmentKey.FUNCTION);
      }
    if (unknownDecls == null)
      {
	unknownDecls = new Hashtable(100);
	decl = null;
      }
    else
      decl = (Declaration) unknownDecls.get(key);
    if (decl == null)
      {
	decl = currentModule.addDeclaration(name);
	decl.setSimple(false);
	decl.setPrivate(true);
	if (function)
	  decl.setProcedureDecl(true);
	if (currentModule.isStatic())
	  decl.setFlag(Declaration.STATIC_SPECIFIED);
	decl.setCanRead(true);
	decl.setCanWrite(true);
        decl.noteValueUnknown();
        // Setting IS_SINGLE_VALUE unconditionally is a kludge.
        // It is OK for Scheme/Lisp, since a variable can't be
        // bound to multiple value.  It is OK for XQuery, since we
        // XQuery doesn't allow unknown/dynamic variables.  FIXME.
	decl.setFlag(Declaration.IS_UNKNOWN|Declaration.IS_SINGLE_VALUE);
	decl.setIndirectBinding(true);
	unknownDecls.put(key, decl);
      }
    return decl;
  }

  protected Expression visitReferenceExp (ReferenceExp exp, Void ignored)
  {
    Declaration decl = exp.getBinding();
    if (decl == null)
      {
	decl = allocUnboundDecl(exp.getSymbol(),
				exp.isProcedureName());
	exp.setBinding(decl);
      }
    if (decl.getFlag(Declaration.IS_UNKNOWN))
      maybeWarnNoDeclarationSeen(exp.getSymbol(), exp.isProcedureName(),
                                 comp, exp);

    capture(exp.contextDecl(), decl, exp);
    return exp;
  }

  void capture(Declaration containing, Declaration decl, ReferenceExp exp)
  {
    Expression dvalue;
    if (decl.isAlias() && (dvalue = decl.getValue()) instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) dvalue;
	Declaration orig = rexp.binding;
	if (orig != null
	    && (containing == null || ! orig.needsContext()))
	  {
            capture(rexp.contextDecl(), orig, null);
	    return;
	  }
      }
    while (decl.isFluid() && decl.context instanceof FluidLetExp)
      {
        decl = decl.base;
      }
    if (containing != null && decl.needsContext())
        capture(containing, null);
    else
        capture(decl, exp);
  }

  protected Expression visitThisExp (ThisExp exp, Void ignored)
  {
    if (exp.isForContext())
      {
        // This is an extension used by define_syntax.
        // FIXME - not really right, but works in simple cases.
        LambdaExp curLambda = getCurrentLambda();
        if (! (curLambda instanceof ModuleExp
               && ((ModuleExp) curLambda).staticInitRun()))
            curLambda.setImportsLexVars();
        return exp;
      }
    else
      return visitReferenceExp(exp, ignored);
  }

  protected Expression visitSetExp (SetExp exp, Void ignored)
  {
    Declaration decl = exp.binding;
    if (decl == null)
      {
	decl = allocUnboundDecl(exp.getSymbol(), exp.isFuncDef());
	exp.binding = decl;
      }
    if (decl.getFlag(Declaration.IS_UNKNOWN))
      maybeWarnNoDeclarationSeen(exp.getSymbol(), false, comp, exp);
    if (! decl.ignorable())
      {
	if (! exp.isDefining())
	  decl = Declaration.followAliases(decl);
	capture(exp.contextDecl(), decl, null);
      }
    return super.visitSetExp(exp, ignored);
  }

}
