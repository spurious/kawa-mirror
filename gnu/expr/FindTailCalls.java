package gnu.expr;

public class FindTailCalls extends ExpFullWalker
{
  public static void findTailCalls (Expression exp)
  {
    FindTailCalls walker = new FindTailCalls();
    exp.walk(walker);
    //or:  walter.walkExpression(exp);
  }

  boolean inTailContext = true;

  public Object walkApplyExp(ApplyExp exp)
  {
    if (inTailContext)
      exp.setTailCall(true);
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	super.walkApplyExp(exp);
	return null;
      }
    finally
      {
	inTailContext = save;
      }
  }

  public Object walkBeginExp(BeginExp exp)
  {
    boolean save = inTailContext;
    try
      {
	int n = exp.exps.length - 1;
	for (int i = 0;  i <= n;  i++)
	  {
	    inTailContext = (i == n);
	    exp.exps[i].walk(this);
	  }
	return null;
      }
    finally
      {
	inTailContext = save;
      }
  }

  public Object walkLetExp (LetExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	int n = exp.inits.length; 
	for (int i = 0;  i < n;  i++) 
	  exp.inits[i].walk(this);
	inTailContext = true;
	exp.body.walk(this);
	return null;
      }
    finally
      {
	inTailContext = save;
      }
  }

  public Object walkIfExp (IfExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	exp.test.walk(this);
	inTailContext = true;
	exp.then_clause.walk(this);
	Expression else_clause = exp.else_clause;
	if (else_clause != null)
	  else_clause.walk(this);
	return null;
      }
    finally
      {
	inTailContext = save;
      }
  }

  public Object walkLambdaExp (LambdaExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	Object result
	  = exp.defaultArgs == null ? null : walkExps(exp.defaultArgs); 
	inTailContext = true;
	if (result == null)
	  result = exp.body.walk(this);
	return result;
      }
    finally
      {
	inTailContext = save;
      }
  }

  public Object walkSetExp (SetExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	super.walkSetExp(exp);
	return null;
      }
    finally
      {
	inTailContext = save;
      }
  }

  public Object walkTryExp (TryExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	super.walkTryExp(exp);
	return null;
      }
    finally
      {
	inTailContext = save;
      }
  }

}
