package gnu.expr;

/** An ExpWalker for doing a complete left-to-right tree walk.
 * Returns the first non-null  walk result. */

public class ExpFullWalker extends ExpWalker
{
  LambdaExp currentLambda = null;

  public final LambdaExp getCurrentLambda() { return currentLambda; }

  public Object walkExps (Expression[] exps)
  {
    int n = exps.length;
    Object result = null;
    for (int i = 0;  i < n && result == null;  i++)
      result = exps[i].walk(this);
    return result;
  }

  public Object walkApplyExp (ApplyExp exp)
  {
    Object result = exp.func.walk(this);
    if (result == null)
      result = walkExps(exp.args);
    return result;
  }

  public Object walkBeginExp (BeginExp exp)
  {
    return walkExps(exp.exps);
  }

  public Object walkIfExp (IfExp exp)
  {
    Object result = exp.test.walk(this);
    if (result == null)
      result = exp.then_clause.walk(this);
    if (result == null)
      result = exp.else_clause.walk(this);
    return result;
  }

  public Object walkLetExp (LetExp exp)
  {
    Object result = null;
    int n = exp.inits.length; 
    for (int i = 0;  i < n && result == null;  i++) 
      result = exp.inits[i].walk(this);
    if (result == null)
      result = exp.body.walk(this);
    return result;
  }

  public Object walkSetExp (SetExp exp)
  {
    return exp.new_value.walk(this);
  }

  public Object walkTryExp (TryExp exp)
  {
    Object result = exp.try_clause.walk(this);
    CatchClause catch_clause = exp.catch_clauses;
    while (result == null && catch_clause != null)
      {
	result = catch_clause.body.walk(this);
	catch_clause = catch_clause.getNext();
      }

    if (result == null)
      result = exp.try_clause.walk(this);
    return result;
  }

  public Object walkLambdaExp (LambdaExp exp)
  {
    LambdaExp save = currentLambda;
    currentLambda = exp;
    try
      {
	Object result = exp.defaultArgs == null ? null
	  : walkExps(exp.defaultArgs);
	if (result == null)
	  result = exp.body.walk(this);
	return result;
      }
    finally
      {
	currentLambda = save;
      }
  }

}
