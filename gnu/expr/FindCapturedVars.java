package gnu.expr;
import java.util.Hashtable;

public class FindCapturedVars extends ExpWalker
{
  public static void findCapturedVars (Expression exp)
  {
    exp.walk(new FindCapturedVars());
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
    if (exp.func instanceof ReferenceExp)
      {
	Declaration decl
	  = Declaration.followAliases(((ReferenceExp) exp.func).binding);
	if (decl != null && decl.context instanceof ModuleExp)
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
	if (unknownDecls != null)
	  {
	    int count = unknownDecls.size();
	    java.util.Enumeration e = unknownDecls.keys();
	    int i = 0;
	    Expression[] init = new Expression[1];
	    LetExp let = new LetExp(init);
	    Declaration env =
	      let.addDeclaration("env$",
				 Compilation.typeEnvironment);
	    init[0] = new ApplyExp(Compilation.getCurrentEnvironmentMethod,
				   Expression.noExpressions);
	    env.setCanRead(true);
	    env.noteValue(init[0]);
	    Expression[] exps = new Expression[count+1];
	    for (;  e.hasMoreElements();  i++)
	      {
		String id = (String) e.nextElement();
		Declaration decl = (Declaration) unknownDecls.get(id);
		Expression[] args = new Expression[2];
		args[0] = new ReferenceExp(env);
		args[1] = new QuoteExp(id);
		SetExp set = new SetExp(decl, 
					new ApplyExp(Compilation.getBindingEnvironmentMethod, args));
		set.setDefining(true);
		exps[i] = set;
	      }
	    exps[i] = currentModule.body;
	    let.setBody(new BeginExp(exps));
	    currentModule.body = let;
	  }
	currentModule = saveModule;
	unknownDecls = saveDecls;
      }
  }

  protected Expression walkFluidLetExp (FluidLetExp exp)
  {
    for (Declaration decl = exp.firstDecl(); decl != null; decl = decl.nextDecl())
      {
	Declaration bind = allocUnboundDecl(decl.getName());
	capture(bind);
	decl.base = bind;
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
	Expression[] inits = exp.inits;
	int len = inits.length;
        BeginExp bexp = (BeginExp) exp.body;
	Expression[] exps = bexp.exps;
	if (bexp.length > len)
	  {
	    int i = 0;
	    Declaration decl = exp.firstDecl();
	    for (; i < len; decl = decl.nextDecl(), i++)
	      {
		if (inits[i] == QuoteExp.nullExp
		    && exps[i] instanceof SetExp)
		  {
		    SetExp set = (SetExp) exps[i];
		    if ((set.new_value instanceof LambdaExp
			 || set.new_value instanceof QuoteExp)
			&& set.binding == decl)
		      {
			inits[i] = set.new_value;
			exps[i] = QuoteExp.voidExp;
		      }
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

    if (decl.getFlag(Declaration.IS_UNKNOWN))
      return; // FIXME - for now, as long as unknows are static
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
    if (Compilation.usingCPStyle())
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
    if (decl.getFlag(Declaration.STATIC_SPECIFIED))
      decl.setSimple(false);
    else if (decl.getCanRead() || declValue == null)
      {
	LambdaExp heapLambda = curLambda;
	heapLambda.setImportsLexVars();
	LambdaExp parent = heapLambda.outerLambda();
	for (LambdaExp outer = parent;  outer != declLambda && outer != null; )
	  {
	    heapLambda = outer;
	    if (! decl.getCanRead() && declValue == outer)
	      break;
	    heapLambda.setNeedsStaticLink();
	    outer = heapLambda.outerLambda();
	  }
	if (decl.base != null)
	  {
	    decl.base.setCanRead(true);
	    capture(decl.base);
	  }
	if (decl.isSimple())
	  {	
	    if (declLambda instanceof ModuleExp
		|| declLambda instanceof ObjectExp)
	      {
		declLambda.heapFrameLambda = declLambda;
	      }
	    else if (declLambda.capturedVars == null)
	      {
		if (! Compilation.usingTailCalls)
		  ;
		else if (heapLambda.isClassGenerated())
		  declLambda.heapFrameLambda = heapLambda;
		else
		  {
		    for (LambdaExp child = declLambda.firstChild; ;
			 child = child.nextSibling)
		      {
			if (child == null)
			  break;
			if (child.isClassGenerated())
			  {
			    declLambda.heapFrameLambda = child;
			    break;
			  }
		      }
		  }
		declLambda.heapFrame = new gnu.bytecode.Variable("heapFrame");
		declLambda.heapFrame.setArtificial(true);
	      }
	    decl.setSimple(false);
	    if (! decl.isPublic())
	      {
		decl.nextCapturedVar = declLambda.capturedVars;
		declLambda.capturedVars = decl;
	      }
	  }
      }
  }

  Hashtable unknownDecls = null;
  ModuleExp currentModule = null;

  Declaration allocUnboundDecl(String name)
  {
    Declaration decl;
    if (unknownDecls == null)
      {
	unknownDecls = new Hashtable(100);
	decl = null;
      }
    else
      decl = (Declaration) unknownDecls.get(name);
    if (decl == null)
      {
	decl = currentModule.addDeclaration(name);
	decl.setSimple(false);
	decl.setPrivate(true);
	if (currentModule.isStatic())
	  decl.setFlag(Declaration.STATIC_SPECIFIED);
	decl.setCanRead(true);
	decl.setFlag(Declaration.IS_UNKNOWN);
	decl.setIndirectBinding(true);
	unknownDecls.put(name, decl);
      }
    return decl;
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl == null)
      {
	decl = allocUnboundDecl(exp.getName());
	exp.setBinding(decl);
      }
    else // FIXME remove else when IS_UNKNOWN decls are non-static
    capture(Declaration.followAliases(decl));
    return exp;
  }

  protected Expression walkThisExp (ThisExp exp)
  {
    // FIXME - not really right, but works in simple cases.
    getCurrentLambda ().setImportsLexVars();
    return exp;
  }

  protected Expression walkSetExp (SetExp exp)
  {
    Declaration decl = exp.binding;
    if (decl == null)
      {
	decl = allocUnboundDecl(exp.getName());
	exp.binding = decl;
      }
    else // FIXME remove else when IS_UNKNOWN decls are non-static
    capture(Declaration.followAliases(decl));
    return super.walkSetExp(exp);
  }

}
