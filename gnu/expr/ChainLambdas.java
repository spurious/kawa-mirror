package gnu.expr;

/** Sets up the firstChild/nextSibling links of each LambdaExp.
 * Setup 'outer' links of ScopeExp and its sub-classes.
 * Also generates a class name for each ClassExp and registers each class.
 * Also, if lambda is bound to a unique declaration, make that its name.
 */

public class ChainLambdas extends ExpExpVisitor<ScopeExp>
{
  public static void chainLambdas (Expression exp, Compilation comp)
  {
    ChainLambdas visitor = new ChainLambdas();
    visitor.setContext(comp);
    visitor.visit(exp, null);
  }

  protected Expression visitScopeExp (ScopeExp exp, ScopeExp scope)
  {
    exp.outer = scope;
    exp.visitChildren(this, exp);
    exp.setIndexes();
    if (exp.mustCompile())
      comp.mustCompileHere();
    return exp;
  }

  protected Expression visitLambdaExp (LambdaExp exp, ScopeExp scope)
  {    
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ClassExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    exp.outer = scope;
    exp.firstChild = null;
    exp.visitChildrenOnly(this, exp);
    exp.visitProperties(this, exp);

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

  protected Expression visitClassExp (ClassExp exp, ScopeExp scope)
  {
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ClassExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    visitScopeExp(exp, scope);

    return exp;
  }
}
