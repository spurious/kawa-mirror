package gnu.expr;

/** An ExpWalker for doing a complete left-to-right tree walk. */

public class ExpFullWalker extends ExpWalker
{
  LambdaExp currentLambda = null;

  /** If exitValue is set to non-null, the walk stops. */
  Object exitValue = null;

  public final LambdaExp getCurrentLambda() { return currentLambda; }

  public Expression[] walkExps (Expression[] exps)
  {
    int n = exps.length;
    for (int i = 0;  i < n && exitValue == null;  i++)
      exps[i] = (Expression) exps[i].walk(this);
    return exps;
  }

  public Object walkExpression (Expression exp) { return exp; }

  public Object walkApplyExp (ApplyExp exp)
  {
    exp.func = (Expression) exp.func.walk(this);
    if (exitValue == null)
      exp.args = walkExps(exp.args);
    return exp;
  }

  public Object walkBeginExp (BeginExp exp)
  {
    exp.exps = walkExps(exp.exps);
    return exp;
  }

  public Object walkIfExp (IfExp exp)
  {
    exp.test = (Expression) exp.test.walk(this);
    if (exitValue == null)
      exp.then_clause = (Expression) exp.then_clause.walk(this);
    if (exitValue == null)
      exp.else_clause = (Expression) exp.else_clause.walk(this);
    return exp;
  }

  public Object walkLetExp (LetExp exp)
  {
    exp.inits = walkExps (exp.inits);
    if (exitValue == null)
      exp.body = (Expression) exp.body.walk(this);
    return exp;
  }

  public Object walkSetExp (SetExp exp)
  {
    exp.new_value = (Expression) exp.new_value.walk(this);
    return exp;
  }

  public Object walkTryExp (TryExp exp)
  {
    exp.try_clause = (Expression) exp.try_clause.walk(this);
    CatchClause catch_clause = exp.catch_clauses;
    while (exitValue == null && catch_clause != null)
      {
	catch_clause.body = (Expression) catch_clause.body.walk(this);
	catch_clause = catch_clause.getNext();
      }

    if (exitValue == null && exp.finally_clause != null)
      exp.finally_clause = (Expression) exp.finally_clause.walk(this);
    return exp;
  }

  public Object walkLambdaExp (LambdaExp exp)
  {
    LambdaExp save = currentLambda;
    currentLambda = exp;
    try
      {
	if (exp.defaultArgs != null)
	  exp.defaultArgs = walkExps(exp.defaultArgs);
	if (exitValue == null)
	  exp.body = (Expression) exp.body.walk(this);
	return exp;
      }
    finally
      {
	currentLambda = save;
      }
  }

}
