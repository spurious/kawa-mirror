package gnu.expr;

/** Sets up the firstChild/nextSibling links of each LambdaExp. */

public class ChainLambdas extends ExpFullWalker
{
  public static void chainLambdas (Expression exp)
  {
    ChainLambdas walker = new ChainLambdas();
    exp.walk(walker);
    //or:  walter.walkExpression(exp);
  }

  public Object walkLambdaExp (LambdaExp exp)
  {    
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ObjectExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    super.walkLambdaExp(exp);

    // Put list of children in proper order.
    LambdaExp prev = null, child = exp.firstChild;
    while (child != null)
      {
	LambdaExp next = child.nextSibling;
	child.nextSibling = prev;
	prev = child;
	child = next;
      }
    exp.firstChild = prev;
    return exp;
  }

  public Object walkObjectExp (ObjectExp exp)
  {    
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ObjectExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    super.walkObjectExp(exp);
    return exp;
  }
}
