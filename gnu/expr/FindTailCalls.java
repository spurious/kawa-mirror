package gnu.expr;

/** Does setTailCall on ApplyExp's that are tail-calls.
    Also setCanRead, setCanCall, setCanWrite on Declarations
    and setCanRead, setCanCall on LambdaExp when appropriate. */

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
	    inTailContext = (i == n) && save;
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
    int n = exp.inits.length; 
    boolean save = inTailContext;
    gnu.bytecode.Variable var;
    try
      {
	inTailContext = false;

	var = exp.firstVar();
	for (int i = 0;  i < n;  i++, var = var.nextVar())
	  exp.inits[i] = walkSetExp ((Declaration) var, exp.inits[i]);
      }
    finally
      {
	inTailContext = save;
      }
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

  public Object walkIfExp (IfExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	exp.test = (Expression) exp.test.walk(this);
      }
    finally
      {
	inTailContext = save;
      }
    exp.then_clause = (Expression) exp.then_clause.walk(this);
    Expression else_clause = exp.else_clause;
    if (else_clause != null)
      exp.else_clause = (Expression) else_clause.walk(this);
    return exp;
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
	inTailContext = false;
	if (exp.defaultArgs != null)
	  exp.defaultArgs = walkExps(exp.defaultArgs);
	inTailContext = true;
	if (exitValue == null && exp.body != null)
	  exp.body = (Expression) exp.body.walk(this);
      }
    finally
      {
	inTailContext = save;
	currentLambda = parent;
      }

    for (LambdaExp child = exp.firstChild;  child != null;
	 child = child.nextSibling)
      {
	if (child.getCanRead()
	    || child.min_args != child.max_args)
	  child.flags |= LambdaExp.CANNOT_INLINE;
	else
	  {
	    ApplyExp caller = child.returnContinuation;
	    if (caller != null && caller != LambdaExp.unknownContinuation
		&& ! Compilation.usingCPStyle())
	      {
		child.setInlineOnly(true);
	      }
	  }
      }

    for (LambdaExp child = exp.firstChild;  child != null;
	 child = child.nextSibling)
      {
	if ((child.flags & (LambdaExp.CANNOT_INLINE|LambdaExp.INLINE_ONLY))!=0)
	  continue;
	// We can inline child if it is a member of a set of functions
	// which can all be inlined in the same method, and for which
	// all callers are known and members of the same,
	// and each function has at most one caller that is not a tail-call.
	// FIXME  Basic algorithm:
	/*
	Vector inlineSet = new Vector();  // empty
	ApplyExp[] apl = (ApplyExp[]) applications.get(child);
	Stack queue = new Stack();
	copy elements of apl to queue;
	while (!queue.empty())
	  {
	    LambdaExp caller = (LambdaExp) queue.pop();
	    if ((caller.flags & LambdaExp.CANNOT_INLINE) != 0)
	      {
		child.flags |= LambdaExp.CANNOT_INLINE;
		break;
	      }
	    if (caller in inlineSet)
	      continue;
	    apl = (ApplyExp[]) applications.get(child);
	    add elements of apl to queue;
	    add caller to inlineSet;
	    add caller.returnContinuation.context to inlineSet;
	  }
	*/
      }
  }

  // Map LambdaExp to ApplyExp[], which is the set of non-self tails
  // calls that call the key.
  // Hashtable applications = new Hashtable();

  public Object walkObjectExp (ObjectExp exp)
  {
    boolean save = inTailContext;
    LambdaExp parent = currentLambda;
    currentLambda = exp;
    exp.setCanRead(true);
    try
      {
	inTailContext = false;
	for (LambdaExp child = exp.firstChild;
	     child != null && exitValue == null;  child = child.nextSibling)
	  walkLambdaExp(child, false);
      }
    finally
      {
	inTailContext = save;
	currentLambda = parent;
      }

    return exp;
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
    if (decl != null && decl.value == value && value instanceof LambdaExp
	&& ! (value instanceof ObjectExp))
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

  public Object walkSynchronizedExp (SynchronizedExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	return super.walkSynchronizedExp(exp);
      }
    finally
      {
	inTailContext = save;
      }
  }
}
