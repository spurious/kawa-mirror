package gnu.expr;

/** Does setTailCall on AppluyExp's that are tail-calls.
    Also sets up the LambdaExp firstChild/nextSibling links.
    Also setCanRead, setCanCall, setCanWrite on Declarations
    and setCanRead, setCancall on LambdaExp when appropriate. */

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
    exp.context = currentLambda;
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	if (exp.func instanceof ReferenceExp)
	  {
	    ReferenceExp func = (ReferenceExp) exp.func;
	    Declaration binding = func.binding;
	    if (binding != null)
	      {
		exp.nextCall = binding.firstCall;
		binding.firstCall = exp;
		binding.setCanCall(true);
	      }
	  }
	else if (exp.func instanceof LambdaExp)
	  {
	    LambdaExp func = (LambdaExp) exp.func;
	    walkLambdaExp(func, false);
	    func.setCanCall(true);
	  }
	else
	  exp.func.walk(this);
	walkExps(exp.args);
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

	gnu.bytecode.Variable var = exp.firstVar();
	for (int i = 0;  i < n;  i++)
	  walkSetExp ((Declaration) var, exp.inits[i]);

	inTailContext = true;
	exp.body.walk(this);

	var = exp.firstVar();
	for (int i = 0;  i < n;  i++, var = var.nextVar())
	  {
	    Declaration decl = (Declaration) var;
	    if (decl.value != null && decl.value instanceof LambdaExp)
	      {
		LambdaExp lexp = (LambdaExp) decl.value;
		if (decl.getCanRead())
		  lexp.setCanRead(true);
		if (decl.getCanCall())
		  lexp.setCanCall(true);
	      }
	  }

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
    return walkLambdaExp (exp, true);
  }

  final Object walkLambdaExp (LambdaExp exp, boolean canRead)
  {
    boolean save = inTailContext;
    LambdaExp parent = currentLambda;
    currentLambda = exp;
    if (canRead)
      exp.setCanRead(true);
    try
      {
	if (parent != null)
	  {
	    currentLambda.nextSibling = parent.firstChild;
	    parent.firstChild = currentLambda;
	    //System.err.println("link "+exp+" to parent "+parent);
	  }
	inTailContext = false;
	Object result
	  = exp.defaultArgs == null ? null : walkExps(exp.defaultArgs); 
	inTailContext = true;
	if (result == null)
	  result = exp.body.walk(this);

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

	return null;
      }
    finally
      {
	inTailContext = save;
	currentLambda = parent;
      }
  }

  public Object walkReferenceExp (ReferenceExp exp)
  {
    if (exp.binding != null)
      exp.binding.setCanRead(true);
    return null;
  }

  final void walkSetExp (Declaration decl, Expression value)
  {
    if (decl != null)
      decl.setCanWrite(true);
    if (decl != null && decl.value == value && value instanceof LambdaExp)
      {
	LambdaExp lexp = (LambdaExp) value; 
	walkLambdaExp(lexp, false);
      }
    else
      value.walk(this);
  }

  public Object walkSetExp (SetExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	walkSetExp(exp.binding, exp.new_value);
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
