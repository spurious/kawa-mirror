package gnu.expr;

/** Does setTailCall on ApplyExp's that are tail-calls.
    Also sets up the LambdaExp firstChild/nextSibling links.
    Also setCanRead, setCanCall, setCanWrite on Declarations
    and setCanRead, setCancall on LambdaExp when appropriate.
    Also do APPLY-LET and APPLY-BEGIN optimizations (described in the code). */

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
    if (exp.func instanceof LetExp) // [APPLY-LET]
      {
	// Optimize ((let (...) body) . args) to (let (...) (body . args)).
	// This helps optimize Scheme "named let".
	LetExp let = (LetExp) exp.func;
	Expression body = let.body;
	let.body = exp;
	exp.func = body;
	return let.walk(this);
      }
    if (exp.func instanceof BeginExp)  // [APPLY-BEGIN]
      {
	// Optimize ((begin ... last) . args) to (begin ... (last . args)).
	// This helps optimize Scheme "named let".
	BeginExp begin = (BeginExp) exp.func;
	Expression[] stmts = begin.exps;
	int last_index = begin.exps.length - 1;
	exp.func = stmts[last_index];
	stmts[last_index] = exp;
	return begin.walk(this);
      }
    if (inTailContext)
      exp.setTailCall(true);
    exp.context = currentLambda;
    boolean save = inTailContext;
    LambdaExp lexp = null;
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
		if (binding.value instanceof LambdaExp)
		  lexp = (LambdaExp) binding.value;
	      }
	  }
	else if (exp.func instanceof LambdaExp)
	  {
	    lexp = (LambdaExp) exp.func;
	    walkLambdaExp(lexp, false);
	    lexp.setCanCall(true);
	  }
	else
	  exp.func = (Expression) exp.func.walk(this);
	if (lexp != null)
	  {
	    if (lexp.returnContinuation == exp) ; // OK
	    else if (lexp == currentLambda && save)
	      ; // (Self-)tail-recursion is OK.
	    else if (lexp.returnContinuation == null)
	      lexp.returnContinuation = exp;
	    else
	      lexp.returnContinuation = LambdaExp.unknownContinuation;
	  }
	exp.args = walkExps(exp.args);
	return exp;
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
	    exp.exps[i] = (Expression) exp.exps[i].walk(this);
	  }
	return exp;
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
	  exp.inits[i] = walkSetExp ((Declaration) var, exp.inits[i]);

	inTailContext = true;
	exp.body = (Expression) exp.body.walk(this);

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

	return exp;
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
	exp.test = (Expression) exp.test.walk(this);
	inTailContext = true;
	exp.then_clause = (Expression) exp.then_clause.walk(this);
	Expression else_clause = exp.else_clause;
	if (else_clause != null)
	  exp.else_clause = (Expression) else_clause.walk(this);
	return exp;
      }
    finally
      {
	inTailContext = save;
      }
  }

  public Object walkLambdaExp (LambdaExp exp)
  {
    walkLambdaExp (exp, true);
    return exp;
  }

  final void walkLambdaExp (LambdaExp exp, boolean canRead)
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
	  }
	inTailContext = false;
	if (exp.defaultArgs != null)
	  exp.defaultArgs = walkExps(exp.defaultArgs);
	inTailContext = true;
	if (exitValue == null)
	  exp.body = (Expression) exp.body.walk(this);

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
    return exp;
  }

  final Expression walkSetExp (Declaration decl, Expression value)
  {
    if (decl != null)
      decl.setCanWrite(true);
    if (decl != null && decl.value == value && value instanceof LambdaExp)
      {
	LambdaExp lexp = (LambdaExp) value; 
	walkLambdaExp(lexp, false);
	return lexp;
      }
    else
      return (Expression) value.walk(this);
  }

  public Object walkSetExp (SetExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	exp.new_value = walkSetExp(exp.binding, exp.new_value);
	return exp;
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
	return super.walkTryExp(exp);
      }
    finally
      {
	inTailContext = save;
      }
  }
}
