package gnu.expr;

/** Class for doing a tree-walk over an Expression tree. */

public class ExpWalker
{
  protected Expression walkExpression (Expression exp)
  {
    exp.walkChildren(this);
    return exp;
  }

  protected Expression walkApplyExp (ApplyExp exp)
  {
    return walkExpression(exp);
  }
  
  protected Expression walkIfExp (IfExp exp)
  {
    return walkExpression(exp);
  }

  protected Expression walkScopeExp (ScopeExp exp)
  {
    return walkExpression(exp);
  }

  protected Expression walkLetExp (LetExp exp) { return walkScopeExp(exp); }
  protected Expression walkLambdaExp (LambdaExp exp) { return walkScopeExp(exp); }
  protected Expression walkClassExp (ClassExp exp) { return walkLambdaExp(exp); }
  protected Expression walkObjectExp (ObjectExp exp) { return walkClassExp(exp); }
  protected Expression walkModuleExp (ModuleExp exp) { return walkLambdaExp(exp); }
  protected Expression walkSetExp (SetExp exp) { return walkExpression(exp); }
  //protected Expression walkSwitchExp (SwitchExp exp) { return walkExpression(exp); }
  protected Expression walkTryExp (TryExp exp) { return walkExpression(exp); }
  protected Expression walkBeginExp (BeginExp exp) { return walkExpression(exp); }
  protected Expression walkQuoteExp (QuoteExp exp) { return walkExpression(exp); }
  protected Expression walkReferenceExp (ReferenceExp exp)
  { return walkExpression(exp); }
  protected Expression walkThisExp (ThisExp exp) { return walkReferenceExp(exp); }
  protected Expression walkSynchronizedExp (SynchronizedExp exp)
    { return walkExpression(exp); }

  protected Expression walkBlockExp(BlockExp exp) { return walkExpression(exp); }
  protected Expression walkExitExp(ExitExp exp) { return walkExpression(exp); }
  protected Expression walkFluidLetExp(FluidLetExp exp)
  {
    return walkLetExp(exp);
  }

  LambdaExp currentLambda = null;

  /** If exitValue is set to non-null, the walk stops. */
  Object exitValue = null;

  public final LambdaExp getCurrentLambda() { return currentLambda; }

  public Expression[] walkExps (Expression[] exps)
  {
    return walkExps(exps, exps.length);
  }

  public Expression[] walkExps (Expression[] exps, int n)
  {
    for (int i = 0;  i < n && exitValue == null;  i++)
      exps[i] = (Expression) exps[i].walk(this);
    return exps;
  }

  public void walkDefaultArgs (LambdaExp exp)
  {
    if (exp.defaultArgs != null)
      exp.defaultArgs = walkExps(exp.defaultArgs);
  }
}
