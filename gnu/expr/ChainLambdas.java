package gnu.expr;

/** Sets up the firstChild/nextSibling links of each LambdaExp.
 * Also generates a class name for each ObjectExp and registers each class.
 * Also, if lambda is bound to a unique declaration, make that its name.
 */

public class ChainLambdas extends ExpFullWalker
{
  Compilation comp;

  public static void chainLambdas (Expression exp, Compilation comp)
  {
    ChainLambdas walker = new ChainLambdas();
    walker.comp = comp;
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

    if (exp.getName() == null && exp.nameDecl != null)
      exp.setName(exp.nameDecl.getName());
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

    // Give name to object class.
    exp.getCompiledClassType(comp);
    comp.addClass(exp.type);
    return exp;
  }
}
