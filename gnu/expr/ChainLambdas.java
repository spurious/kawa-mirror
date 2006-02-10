package gnu.expr;

/** Sets up the firstChild/nextSibling links of each LambdaExp.
 * Setup 'outer' links of ScopeExp and its sub-classes.
 * Also generates a class name for each ClassExp and registers each class.
 * Also, if lambda is bound to a unique declaration, make that its name.
 */

public class ChainLambdas extends ExpWalker
{
  ScopeExp currentScope;

  public static void chainLambdas (Expression exp, Compilation comp)
  {
    ChainLambdas walker = new ChainLambdas();
    walker.setContext(comp);
    walker.walk(exp);
  }

  protected Expression walkScopeExp (ScopeExp exp)
  {
    ScopeExp saveScope = currentScope;
    try
      {
	exp.outer = currentScope;
	currentScope = exp;
	exp.walkChildren(this);
        exp.setIndexes();
        if (exp.mustCompile())
          comp.mustCompileHere();
	return exp;
      }
    finally
      {
	currentScope = saveScope;
      }
  }

  protected Expression walkLambdaExp (LambdaExp exp)
  {    
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ClassExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    ScopeExp saveScope = currentScope;
    try
      {
	exp.outer = currentScope;
        exp.firstChild = null;
	currentScope = exp;
	exp.walkChildrenOnly(this);
      }
    finally
      {
	currentScope = saveScope;
      }
    exp.walkProperties(this);

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
    exp.setIndexes();
    if (exp.mustCompile())
      comp.mustCompileHere();
    return exp;
  }

  protected Expression walkClassExp (ClassExp exp)
  {
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ClassExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    walkScopeExp(exp);

    return exp;
  }
}
