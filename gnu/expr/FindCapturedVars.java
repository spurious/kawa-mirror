package gnu.expr;

public class FindCapturedVars extends ExpFullWalker
{
  public static void findCapturedVars (Expression exp)
  {
    exp.walk(new FindCapturedVars());
  }

  public void capture(Declaration decl)
  {
    LambdaExp curLambda = getCurrentLambda ();
    LambdaExp declLambda = decl.getContext().currentLambda ();
    while (curLambda != declLambda && curLambda.getInlineOnly())
      curLambda = curLambda.outerLambda();
    if (curLambda != declLambda)
      {
	LambdaExp heapLambda = curLambda;
	for (;;)
	  {
	    heapLambda.setImportsLexVars(true);
	    LambdaExp outer = heapLambda.outerLambda();
	    if (outer == declLambda)
	      break;
	    heapLambda = outer;
	  }
	if (decl.isSimple())
	  {
	    if (declLambda.heapFrameLambda == null)
	      {
		if (heapLambda.getInlineOnly())
		  {
		    // First look for other non-line siblings of heapLambda,
		    // It that fails, create a dummy LambdaExp.  FIXME
		    throw new Error("not implemented - capture thru inline");
		  }
		declLambda.heapFrameLambda = heapLambda;
		declLambda.heapFrame
		  = declLambda.addDeclaration("heapFrame",
					      Compilation.objArrayType);
		declLambda.heapFrame.setArtificial(true);
	      }
	    else
	      heapLambda = declLambda.heapFrameLambda;
	    decl.setSimple(false);
	    decl.nextCapturedVar = heapLambda.capturedVars;
	    heapLambda.capturedVars = decl;
	  }
      }
  }

  public Object walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl != null)
      capture(decl);
   return null;
  }

  public Object walkSetExp (SetExp exp)
  {
    if (exp.binding != null)
      capture(exp.binding);
    return super.walkSetExp(exp);
  }

}
