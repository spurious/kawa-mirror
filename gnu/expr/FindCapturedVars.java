package gnu.expr;

public class FindCapturedVars extends ExpFullWalker
{
  public static void findCapturedVars (Expression exp)
  {
    exp.walk(new FindCapturedVars());
  }

  public Object walkLetExp (LetExp exp)
  {
    if (exp.body instanceof BeginExp)
      {
	// Optimize "letrec"-like forms.
	// If init[i] is the magic QuoteExp.nullExp, and the real value
	// is a LambdaExp or a QuoteExp, we're not going to get weird
	// order-dependencies, and it is safe to transform it to a regular let.
	Expression[] inits = exp.inits;
	int len = inits.length;
	Expression[] exps = ((BeginExp) exp.body).exps;
	if (exps.length > len)
	  {
	    int i = 0;
	    gnu.bytecode.Variable var = exp.firstVar ();
	    for (; i < len; var = var.nextVar (), i++)
	      {
		Declaration decl = (Declaration) var;
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
    if (decl.isStatic() || ! (decl.getCanRead() || decl.getCanCall()))
      return;
    LambdaExp curLambda = getCurrentLambda ();
    LambdaExp declLambda = decl.getContext().currentLambda ();

    // If curLambda is inlined, the function that actually needs a closure
    // is its caller.  We get its caller using returnContinuation.context.
    // A complication is that we can have a chain of functions that
    // recursively call each other, and are hence inlined in each other.
    // Since function is only inlined if it has a single call site,
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
    if (curLambda == declLambda)
      return;

    // The logic here is similar to that of decl.ignorable():
    LambdaExp declValue;
    if (decl.value == null || ! (decl.value instanceof LambdaExp))
      declValue = null;
    else
      {
        declValue = (LambdaExp) decl.value;
        if (declValue.getInlineOnly())
          return;
        if (declValue.isHandlingTailCalls())
          declValue = null;
        else if (declValue == curLambda && ! decl.getCanRead())
          return;
      }
    if (decl.getCanRead() || declValue == null)
      {
	LambdaExp heapLambda = curLambda;
	heapLambda.setImportsLexVars();
	LambdaExp parent = heapLambda.outerLambda();
	for (LambdaExp outer = parent;  outer != declLambda; )
	  {
	    heapLambda = outer;
	    if (! decl.getCanRead() && declValue == outer)
	      break;
	    heapLambda.setNeedsStaticLink();
	    outer = heapLambda.outerLambda();
	  }
	if (decl.isSimple())
	  {
	    if (declLambda.capturedVars == null)
	      {
		if (heapLambda.isClassGenerated())
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
		declLambda.heapFrame
		  = declLambda.addDeclaration("heapFrame");
		declLambda.heapFrame.setArtificial(true);
	      }
	    decl.setSimple(false);
	    decl.nextCapturedVar = declLambda.capturedVars;
	    declLambda.capturedVars = decl;
	  }
      }
  }

  public Object walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl != null)
      capture(decl);
   return exp;
  }

  public Object walkSetExp (SetExp exp)
  {
    if (exp.binding != null)
      capture(exp.binding);
    return super.walkSetExp(exp);
  }

}
