package gnu.expr;

public class FindCapturedVars extends ExpFullWalker
{
  public static void findCapturedVars (Expression exp)
  {
    exp.walk(new FindCapturedVars());
  }

  public Object walkLambdaExp (LambdaExp exp)
  {
    if (! exp.getCanRead())
      {
	ApplyExp caller = exp.returnContinuation;
	if (caller != null && caller != LambdaExp.unknownContinuation
	    && exp.min_args == exp.max_args)
	  exp.setInlineOnly(true);
      }
    return super.walkLambdaExp (exp);
  }

  public void capture(Declaration decl)
  {
    LambdaExp curLambda = getCurrentLambda ();
    LambdaExp declLambda = decl.getContext().currentLambda ();
    while (curLambda != declLambda && curLambda.getInlineOnly())
      curLambda = curLambda.outerLambda();
    if (curLambda != declLambda && ! decl.ignorable())
      {
	LambdaExp heapLambda = curLambda;
	for (;;)
	  {
	    heapLambda.setImportsLexVars();
	    LambdaExp outer = heapLambda.outerLambda();
	    if (outer == declLambda)
	      break;
	    heapLambda = outer;
	  }
	if (decl.isSimple())
	  {
	    if (declLambda.capturedVars == null)
	      {
		if (! heapLambda.getInlineOnly() && heapLambda.getCanRead())
		  declLambda.heapFrameLambda = heapLambda;
		else
		  {
		    for (LambdaExp child = declLambda.firstChild; ;
			 child = child.nextSibling)
		      {
			if (child == null)
			  break;
			if (! child.getInlineOnly() && child.getCanRead())
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
