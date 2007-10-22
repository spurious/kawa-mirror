// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import java.util.Hashtable;
import java.io.Externalizable;
import gnu.bytecode.Type;
import gnu.mapping.*;

public class FindCapturedVars extends ExpWalker
{
  public static void findCapturedVars (Expression exp, Compilation comp)
  {
    FindCapturedVars walker = new FindCapturedVars();
    walker.setContext(comp);
    exp.walk(walker);
  }

  protected Expression walkApplyExp (ApplyExp exp)
  {
    boolean skipFunc = false;
    // If the func is bound to a module-level known function, and it
    // doesn't need a closure yet (i.e. could be compiled to a static
    // method), don't walk the function, since that might force it to
    // unnecessarily get "captured" which might force the current
    // function to require a closure.  That would be wasteful if the
    // alternative is to just call func using invokestatic.  (It is
    // possible that we later find out that func needs a static link,
    // in which case the current function does as well;  this is taken
    // care of by calling setCallersNeedStaticLink in LambdaExp.)
    if (exp.func instanceof ReferenceExp
	&& Compilation.defaultCallConvention <= Compilation.CALL_WITH_RETURN)
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
    // avoid walking the type argument.
    else if (exp.func instanceof QuoteExp && exp.getArgCount() > 0)
      {
        Object val = ((QuoteExp) exp.func).getValue();
        Expression arg0 = exp.getArg(0);
        if (val instanceof PrimProcedure && arg0 instanceof ReferenceExp)
          {
            PrimProcedure pproc = (PrimProcedure) val;
            Declaration decl
              = Declaration.followAliases(((ReferenceExp) arg0).binding);
            if (decl != null && decl.context instanceof ModuleExp
                && ! decl.getFlag(Declaration.NONSTATIC_SPECIFIED))
              {
                Expression value = decl.getValue();
                if (value instanceof ClassExp)
                  {
                    Expression[] args = exp.getArgs();
                    LambdaExp lexp = (LambdaExp) value;
                    if (! lexp.getNeedsClosureEnv())
                      {
                        exp.nextCall = decl.firstCall;
                        decl.firstCall = exp;
                        for (int i = 1;  i < args.length;  i++)
                          args[i].walk(this);
                        return exp;
                      }
                  }
              }
          }
      }
    if (! skipFunc)
      exp.func = (Expression) exp.func.walk(this);
    if (exitValue == null)
      exp.args = walkExps(exp.args);
    return exp;
  }

  public void walkDefaultArgs (LambdaExp exp)
  {
    if (exp.defaultArgs == null)
      return;

    super.walkDefaultArgs(exp);

    // Check if any default expression "captured" a parameters.
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

  protected Expression walkClassExp (ClassExp exp)
  {
    Expression ret = super.walkClassExp(exp);
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
    if (exp.isSimple() && exp.getNeedsClosureEnv())
      {
        comp.error('w', "simple class requiring lexical link - use define-class instead");
        //exp.setSimple(false);
        if (exp.nameDecl != null
            && exp.nameDecl.getType() == Compilation.typeClass)
          exp.nameDecl.setType(Compilation.typeClassType);
      }
    return ret;
  }

  protected Expression walkModuleExp (ModuleExp exp)
  {
    ModuleExp saveModule = currentModule;
    Hashtable saveDecls = unknownDecls;
    currentModule = exp;
    unknownDecls = null;
    try
      {
	return walkLambdaExp(exp);
      }
    finally
      {
	currentModule = saveModule;
	unknownDecls = saveDecls;
      }
  }

  protected Expression walkFluidLetExp (FluidLetExp exp)
  {
    for (Declaration decl = exp.firstDecl(); decl != null; decl = decl.nextDecl())
      {
        if (decl.base == null)
          {
            Declaration bind = allocUnboundDecl(decl.getSymbol(), false);
            capture(bind);
            decl.base = bind;
          }
      }
    return super.walkLetExp(exp);
  }

  protected Expression walkLetExp (LetExp exp)
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
	Expression[] inits = exp.inits;
	int len = inits.length;
	Expression[] exps = ((BeginExp) exp.body).exps;
	int init_index = 0;
	Declaration decl = exp.firstDecl();
	for (int begin_index = 0;
	     begin_index < exps.length && init_index < len;
	     begin_index++)
	  {
	    Expression st = exps[begin_index];
	    if (st instanceof SetExp)
	      {
		SetExp set = (SetExp) st;
		if (set.binding == decl
		    && inits[init_index] == QuoteExp.nullExp
		    && set.isDefining())
		  {
		    Expression new_value = set.new_value;
		    if ((new_value instanceof QuoteExp
			 || new_value instanceof LambdaExp)
			&& decl.getValue() == new_value)
		      {
			inits[init_index] = new_value;
			exps[begin_index] = QuoteExp.voidExp;
		      }
		    init_index++;
		    decl = decl.nextDecl();
		  }
	      }
	  }
      }
    return super.walkLetExp(exp);
  }

  public void capture(Declaration decl)
  {
    if (! (decl.getCanRead() || decl.getCanCall()))
      return;
    if (decl.field != null && decl.field.getStaticFlag())
      return;

    LambdaExp curLambda = getCurrentLambda ();
    LambdaExp declLambda = decl.getContext().currentLambda ();

    // If curLambda is inlined, the function that actually needs a closure
    // is its caller.  We get its caller using returnContinuation.context.
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
        ApplyExp curReturn = curLambda.returnContinuation;
        if (chain == null || curReturn == null)
          {
            // Infinite loop of functions that are inlined in each other.
            curLambda.setCanCall(false);
            return;
        }
        curLambda = curReturn.context;
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
	capture(decl.base);
      }
    else if (decl.getCanRead() || declValue == null)
      {
	if (! decl.isStatic())
	  {
	    LambdaExp heapLambda = curLambda;
	    heapLambda.setImportsLexVars();
	    LambdaExp parent = heapLambda.outerLambda();
	    for (LambdaExp outer = parent;  outer != declLambda && outer != null; )
	      {
		heapLambda = outer;
		if (! decl.getCanRead() && declValue == outer)
		  break;
		Declaration heapDecl = heapLambda.nameDecl;
		if (heapDecl != null
		    && heapDecl.getFlag(Declaration.STATIC_SPECIFIED))
		  {
		    comp.error('e', "static " + heapLambda.getName()
			       + " references non-static " + decl.getName());
		  }
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
	decl.setFlag(Declaration.IS_UNKNOWN);
	decl.setIndirectBinding(true);
	unknownDecls.put(key, decl);
      }
    return decl;
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl == null)
      {
	decl = allocUnboundDecl(exp.getSymbol(),
				exp.isProcedureName());
	exp.setBinding(decl);
      }
    if (decl.getFlag(Declaration.IS_UNKNOWN))
      {
	if (comp.getBooleanOption("warn-undefined-variable", false))
	  {
	    Object resolved
	      = comp.resolve(exp.getSymbol(), exp.isProcedureName());
	    if (resolved == null)
	      comp.error('w', "no declaration seen for "+exp.getName(), exp);
	  }
      }

    capture(exp.contextDecl(), decl);
    return exp;
  }

  void capture (Declaration containing, Declaration decl)
  {
    if (decl.isAlias() && decl.value instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) decl.value;
	Declaration orig = rexp.binding;
	if (orig != null
	    && (containing == null || ! orig.needsContext()))
	  {
	    capture(rexp.contextDecl(), orig);
	    return;
	  }
      }
    if (containing != null && decl.needsContext())
      capture(containing);
    else
      capture(decl);
  }

  protected Expression walkThisExp (ThisExp exp)
  {
    if (exp.isForContext())
      {
        // This is an extension used by define_syntax.
        // FIXME - not really right, but works in simple cases.
        getCurrentLambda ().setImportsLexVars();
        return exp;
      }
    else
      return walkReferenceExp (exp);
  }

  protected Expression walkSetExp (SetExp exp)
  {
    Declaration decl = exp.binding;
    if (decl == null)
      {
	decl = allocUnboundDecl(exp.getSymbol(), exp.isFuncDef());
	exp.binding = decl;
      }
    if (! decl.ignorable())
      {
	if (! exp.isDefining())
	  decl = Declaration.followAliases(decl);
	capture(exp.contextDecl(), decl);
      }
    return super.walkSetExp(exp);
  }

}
