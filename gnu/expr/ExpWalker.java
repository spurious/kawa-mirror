package gnu.expr;

public class ExpWalker
{
  public Object walkExpression (Expression exp) { return null; }
  public Object walkApplyExp (ApplyExp exp) { return walkExpression(exp); }
  public Object walkSetApplyExp (SetApplyExp exp) { return walkApplyExp(exp); }
  public Object walkIfExp (IfExp exp) { return walkExpression(exp); }
  public Object walkScopeExp (ScopeExp exp) { return walkExpression(exp); }
  public Object walkLetExp (LetExp exp) { return walkScopeExp(exp); }
  public Object walkLambdaExp (LambdaExp exp) { return walkScopeExp(exp); }
  //public Object walkModuleExp (ModuleExp exp) { return walkLambdaExp(exp); }
  public Object walkSetExp (SetExp exp) { return walkExpression(exp); }
  //public Object walkSwitchExp (SwitchExp exp) { return walkExpression(exp); }
  public Object walkTryExp (TryExp exp) { return walkExpression(exp); }
  public Object walkBeginExp (BeginExp exp) { return walkExpression(exp); }
  public Object walkQuoteExp (QuoteExp exp) { return walkExpression(exp); }
  public Object walkReferenceExp (ReferenceExp exp)
  { return walkExpression(exp); }
  public Object walkSynchronizedExp (SynchronizedExp exp)
    { return walkExpression(exp); }
}
