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
    if (decl.isStatic())
      return;
    LambdaExp curLambda = getCurrentLambda ();
    LambdaExp declLambda = decl.getContext().currentLambda ();
    while (curLambda != declLambda && curLambda.getInlineOnly())
      curLambda = curLambda.returnContinuation.context;
    if (curLambda == declLambda)
      return;

    if (decl.getCanRead() || decl.getCanCall())
      {
	// The logic here is similar to that of decl.ignorable():
	LambdaExp declValue;
	if (decl.value == null || ! (decl.value instanceof LambdaExp))
	  declValue = null;
	else
	  {
	    declValue = (LambdaExp) decl.value;
	    if (declValue.isHandlingTailCalls() && !declValue.getInlineOnly())
	      declValue = null;
	  }

	LambdaExp heapLambda = curLambda;
	heapLambda.setImportsLexVars();
	LambdaExp parent = heapLambda.outerLambda();
	for (LambdaExp outer = parent;  outer != declLambda; )
	  {
	    heapLambda = outer;
	    if (! decl.getCanRead() && declValue == outer)
	      break;
	    heapLambda.setNeedsStaticLink(true);
	    outer = heapLambda.outerLambda();
	  }
	if (decl.isSimple() && (decl.getCanRead() || declValue == null))
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
