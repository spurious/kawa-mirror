package gnu.expr;

/** Does setTailCall on ApplyExp's that are tail-calls.
    Also setCanRead, setCanCall, setCanWrite on Declarations
    and setCanRead, setCanCall on LambdaExp when appropriate. */

public class FindTailCalls extends ExpWalker
{
  public static void findTailCalls (Expression exp, Compilation comp)
  {
    FindTailCalls walker = new FindTailCalls();
    walker.setContext(comp);
    walker.walk(exp);
  }

  boolean inTailContext = true;

  protected Expression walkApplyExp(ApplyExp exp)
  {
    if (inTailContext)
      exp.setTailCall(true);
    exp.context = currentLambda;
    boolean save = inTailContext;
    LambdaExp lexp = null;
    try
      {
	inTailContext = false;
	boolean isAppendValues = false;
	if (exp.func instanceof ReferenceExp)
	  {
	    ReferenceExp func = (ReferenceExp) exp.func;
	    Declaration binding = Declaration.followAliases(func.binding);
	    if (binding != null)
	      {
		exp.nextCall = binding.firstCall;
		binding.firstCall = exp;
		binding.setCanCall();
		Expression value = binding.getValue();
		if (value instanceof LambdaExp)
		  lexp = (LambdaExp) value;
	      }
	  }
	else if (exp.func instanceof LambdaExp
		 && ! (exp.func instanceof ClassExp))
	  {
	    lexp = (LambdaExp) exp.func;
	    walkLambdaExp(lexp, false);
	    lexp.setCanCall(true);
	  }
	else if (exp.func instanceof QuoteExp
		 && (((QuoteExp) exp.func).getValue()
		     == gnu.kawa.functions.AppendValues.appendValues))
	  isAppendValues = true;
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
	if (isAppendValues && exp.args.length > 0)
	  {
	    int last = exp.args.length - 1;
	    exp.args = walkExps(exp.args, last);
	    inTailContext = save;
	    exp.args[last] = walk(exp.args[last]);
	  }
	else
	  exp.args = walkExps(exp.args);
	return exp;
      }
    finally
      {
	inTailContext = save;
      }
  }

  protected Expression walkBeginExp(BeginExp exp)
  {
    boolean save = inTailContext;
    try
      {
	int n = exp.length - 1;
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

  protected Expression walkFluidLetExp (FluidLetExp exp)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null; decl = decl.nextDecl())
      {
        decl.setCanRead(true);
        decl.setCanWrite(true);
      }
    boolean save = inTailContext;
    inTailContext = false;
    try
      {
	return super.walkFluidLetExp(exp);
      }
    finally
      {
	inTailContext = save;
      }
  }

  protected Expression walkLetExp (LetExp exp)
  {
    int n = exp.inits.length; 
    boolean save = inTailContext;
    Declaration decl;
    try
      {
	inTailContext = false;

	decl = exp.firstDecl();
	for (int i = 0;  i < n;  i++, decl = decl.nextDecl())
	  exp.inits[i] = walkSetExp (decl, exp.inits[i]);
      }
    finally
      {
	inTailContext = save;
      }
    exp.body = (Expression) exp.body.walk(this);
    walkDecls(exp);
    return exp;
  }

  public void walkDecls (ScopeExp exp)
  {
    Declaration decl = exp.firstDecl();
    for (;  decl != null;  decl = decl.nextDecl())
      {
	Expression value = decl.getValue();
	if (value != null && value instanceof LambdaExp)
	  {
	    LambdaExp lexp = (LambdaExp) value;
	    if (decl.getCanRead())
	      lexp.setCanRead(true);
	    if (decl.getCanCall())
	      lexp.setCanCall(true);
	  }
      }
  }

  protected Expression walkIfExp (IfExp exp)
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

  protected Expression walkLambdaExp (LambdaExp exp)
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
	inTailContext = exp.getInlineOnly() ? save : true;
	if (exitValue == null && exp.body != null)
	  exp.body = (Expression) exp.body.walk(this);
      }
    finally
      {
	inTailContext = save;
	currentLambda = parent;
      }

    walkDecls(exp);

    for (LambdaExp child = exp.firstChild;  child != null;
	 child = child.nextSibling)
      {
	if (child.getCanRead()
	    || child.isClassMethod()
	    || child.min_args != child.max_args)
	  child.flags |= LambdaExp.CANNOT_INLINE;
	else
	  {
	    ApplyExp caller = child.returnContinuation;
	    if (caller != LambdaExp.unknownContinuation
		&& ! comp.usingCPStyle())
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
	// all callers are known and members of the same set,
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

  protected Expression walkClassExp (ClassExp exp)
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

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = Declaration.followAliases(exp.binding);
    if (decl != null)
      decl.setCanRead(true);
    return exp;
  }

  final Expression walkSetExp (Declaration decl, Expression value)
  {
    if (decl != null)
      decl.setCanWrite();
    if (decl != null && decl.getValue() == value
	&& value instanceof LambdaExp && ! (value instanceof ClassExp)
        && ! decl.isPublic())
      {
	LambdaExp lexp = (LambdaExp) value; 
	walkLambdaExp(lexp, false);
	return lexp;
      }
    else
      return (Expression) value.walk(this);
  }

  protected Expression walkSetExp (SetExp exp)
  {
    boolean save = inTailContext;
    try
      {
	inTailContext = false;
	Expression declValue;
	Declaration decl = exp.binding;
	if (decl != null && decl.isAlias())
	  {
	    if (exp.isDefining())
	      {
		exp.new_value = (Expression) exp.new_value.walk(this);
		return exp;
	      }
	    decl = Declaration.followAliases(decl);

	  }
	exp.new_value = walkSetExp(decl, exp.new_value);
	return exp;
      }
    finally
      {
	inTailContext = save;
      }
  }

  protected Expression walkTryExp (TryExp exp)
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

  protected Expression walkSynchronizedExp (SynchronizedExp exp)
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
