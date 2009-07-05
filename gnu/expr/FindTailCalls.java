package gnu.expr;
import gnu.bytecode.Type;

/** Does setTailCall on ApplyExp's that are tail-calls.
 * Also setCanRead, setCanCall on Declarations
 * and setCanRead, setCanCall on LambdaExp when appropriate.
 * (setCanWrite on Declarations needs to be set before this.)
 * Note the final part of deciding inlineability has to be done after
 * FindTailCalls finishes (or at least after we've walked all possible
 * callers), so it is deferred to FindCapturedvars.walkLambdaExp.
 */

public class FindTailCalls extends ExpWalker
{
  public static void findTailCalls (Expression exp, Compilation comp)
  {
    FindTailCalls walker = new FindTailCalls();
    walker.setContext(comp);
    walker.walk(exp);
  }

  Expression returnContinuation;

  public boolean inTailContext ()
  {
    return returnContinuation == currentLambda.body;
  }

  protected Expression walkExpression (Expression exp)
  {
    Expression saveContext = returnContinuation;
    returnContinuation = exp;
    Expression ret = super.walkExpression(exp);
    returnContinuation = saveContext;
    return ret;
  }

  public Expression[] walkExps (Expression[] exps, int n)
  {
    Expression saveContext = returnContinuation;
    for (int i = 0;  i < n;  i++)
      {
        Expression expi = exps[i];
        returnContinuation = expi;
        exps[i] = walk(expi);
      }
    returnContinuation = saveContext;
    return exps;
  }

  protected Expression walkApplyExp(ApplyExp exp)
  {
    boolean inTailContext = inTailContext();
    if (inTailContext)
      exp.setTailCall(true);
    exp.context = currentLambda;
    LambdaExp lexp = null;
    boolean isAppendValues = false;
    if (exp.func instanceof ReferenceExp)
      {
        ReferenceExp func = (ReferenceExp) exp.func;
        Declaration binding = Declaration.followAliases(func.binding);
        if (binding != null)
          {
            // No point in building chain if STATIC_SPECIFIED, and it can
            // lead to memory leaks.  At least if interactive calls cam
            // resolve to previously-compiled Declarations (as in XQuery).
            if (! binding.getFlag(Declaration.STATIC_SPECIFIED))
              {
                exp.nextCall = binding.firstCall;
                binding.firstCall = exp;
              }
            Compilation comp = getCompilation();
            binding.setCanCall();
            if (! comp.mustCompile)
              // Avoid tricky optimization if we're interpreting.
              binding.setCanRead();
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
      {
        Expression saveContext = returnContinuation;
        returnContinuation = exp.func;
        exp.func = exp.func.walk(this);
        returnContinuation = saveContext;
      }
    if (lexp != null)
      {
        if (lexp.returnContinuation == returnContinuation) ; // OK
        else if (lexp == currentLambda && inTailContext)
          ; // (Self-)tail-recursion is OK.
        else if (inTailContext)
          {
            if (lexp.tailCallers == null)
              lexp.tailCallers = new java.util.HashSet();
            lexp.tailCallers.add(currentLambda);
          }
        else if (lexp.returnContinuation == null)
          {
            lexp.returnContinuation = returnContinuation;
            lexp.inlineHome = currentLambda;
          }
        else
          {
            lexp.returnContinuation = LambdaExp.unknownContinuation;
            lexp.inlineHome = null;
          }
      }
    if (isAppendValues
        && currentLambda.getCallConvention() >= Compilation.CALL_WITH_CONSUMER)
     {
       Expression[] args = exp.args;
       int nargs = args.length;
       for (int i = 0;  i < nargs;  i++)
         {
           args[i] = walk(args[i]);
         }
      }
    else
      exp.args = walkExps(exp.args);
    return exp;
  }

  protected Expression walkBlockExp(BlockExp exp)
  {
    Expression saveContext = returnContinuation;
    try
      {
        exp.body = exp.body.walk(this);
        if (exp.exitBody != null)
          {
            returnContinuation = exp.exitBody;
            exp.exitBody = exp.exitBody.walk(this);
          }
        return exp;
      }
    finally
      {
        returnContinuation = saveContext;
      }
  }

  protected Expression walkBeginExp(BeginExp exp)
  {
    Expression saveContext = returnContinuation;
    try
      {
	int n = exp.length - 1;
	for (int i = 0;  i <= n;  i++)
	  {
            returnContinuation = i == n ? saveContext : exp.exps[i];
	    exp.exps[i] = (Expression) exp.exps[i].walk(this);
	  }
	return exp;
      }
    finally
      {
        returnContinuation = saveContext;
      }
  }

  protected Expression walkFluidLetExp (FluidLetExp exp)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null; decl = decl.nextDecl())
      {
        decl.setCanRead(true);
        if (decl.base != null)
          decl.base.setCanRead(true);
      }
    Expression saveContext = returnContinuation;
    try
      {
        walkLetDecls(exp);
        returnContinuation = exp.body;
        exp.body = (Expression) exp.body.walk(this);
      }
    finally
      {
        returnContinuation = saveContext;
      }
    postWalkDecls(exp);
    return exp;
  }

  void walkLetDecls (LetExp exp)
  {
        Declaration decl = exp.firstDecl();
        int n = exp.inits.length; 
	for (int i = 0;  i < n;  i++, decl = decl.nextDecl())
	  {
	    Expression init = walkSetExp (decl, exp.inits[i]);
	    // Optimize letrec-like forms.
	    if (init == QuoteExp.undefined_exp)
	      {
		Expression value = decl.getValue();
		if (value instanceof LambdaExp
		    || (value != init && value instanceof QuoteExp))
		  init = value;
	      }
	    exp.inits[i] = init;
	  }
  }

  protected Expression walkLetExp (LetExp exp)
  {
    Expression saveContext = returnContinuation;
    try
      {
        walkLetDecls(exp);
      }
    finally
      {
        returnContinuation = saveContext;
      }
    exp.body = (Expression) exp.body.walk(this);
    postWalkDecls(exp);
    return exp;
  }

  public void postWalkDecls (ScopeExp exp)
  {
    Declaration decl = exp.firstDecl();
    for (;  decl != null;  decl = decl.nextDecl())
      {
	Expression value = decl.getValue();
	if (value instanceof LambdaExp)
	  {
	    LambdaExp lexp = (LambdaExp) value;
	    if (decl.getCanRead())
	      lexp.setCanRead(true);
	    if (decl.getCanCall())
	      lexp.setCanCall(true);
	  }
        if (decl.getFlag(Declaration.EXPORT_SPECIFIED)
            && value instanceof ReferenceExp)
          {
            ReferenceExp rexp = (ReferenceExp) value;
            Declaration context = rexp.contextDecl();
            if (context != null && context.isPrivate())
              context.setFlag(Declaration.EXTERNAL_ACCESS);
          }
      }
  }

  protected Expression walkIfExp (IfExp exp)
  {
    Expression saveContext = returnContinuation;
    try
      {
        returnContinuation = exp.test;
	exp.test = (Expression) exp.test.walk(this);
      }
    finally
      {
        returnContinuation = saveContext;
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
    Expression saveContext = returnContinuation;
    returnContinuation = exp;
    LambdaExp parent = currentLambda;
    currentLambda = exp;
    if (canRead)
      exp.setCanRead(true);
    try
      {
        returnContinuation = exp;
	if (exp.defaultArgs != null)
	  exp.defaultArgs = walkExps(exp.defaultArgs);
        returnContinuation = exp.getInlineOnly() ? saveContext : exp.body;
	if (exitValue == null && exp.body != null)
	  exp.body = (Expression) exp.body.walk(this);
      }
    finally
      {
        returnContinuation = saveContext;
	currentLambda = parent;
      }

    postWalkDecls(exp);
  }


  // Map LambdaExp to ApplyExp[], which is the set of non-self tails
  // calls that call the key.
  // Hashtable applications = new Hashtable();

  protected Expression walkClassExp (ClassExp exp)
  {
    Expression saveContext = returnContinuation;
    returnContinuation = exp;
    LambdaExp parent = currentLambda;
    currentLambda = exp;
    exp.setCanRead(true);
    try
      {
	for (LambdaExp child = exp.firstChild;
	     child != null && exitValue == null;  child = child.nextSibling)
	  walkLambdaExp(child, false);
      }
    finally
      {
        returnContinuation = saveContext;
	currentLambda = parent;
      }

    return exp;
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = Declaration.followAliases(exp.binding);
    if (decl != null)
      {
        // Replace references to a void variable (including one whose value
        // is the empty sequence in XQuery) by an empty constant.  This is
        // not so much an optimization as avoiding the complications and
        // paradoxes of variables and expression that are void.
        Type type = decl.type;
        if (type != null && type.isVoid())
          return QuoteExp.voidExp;
        decl.setCanRead(true);
      }
    Declaration ctx = exp.contextDecl();
    if (ctx != null)
      ctx.setCanRead(true);
    return exp;
  }

  final Expression walkSetExp (Declaration decl, Expression value)
  {
    returnContinuation = value;
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
    Expression saveContext = returnContinuation;
    try
      {
	Declaration decl = exp.binding;
	if (decl != null && decl.isAlias())
	  {
	    if (exp.isDefining())
	      {
                returnContinuation = exp.new_value;
		exp.new_value = (Expression) exp.new_value.walk(this);
		return exp;
	      }
	    decl = Declaration.followAliases(decl);
	  }
        Declaration ctx = exp.contextDecl();
        if (ctx != null)
          ctx.setCanRead(true);
	Expression value = walkSetExp(decl, exp.new_value);
	if (decl != null && decl.context instanceof LetExp
	    && value == decl.getValue()
	    && (value instanceof LambdaExp || value instanceof QuoteExp))
	  {
	    // The assignment is redundant, as it has been moved to the
	    // initialization of the LetExp.
	    return QuoteExp.voidExp;
	  }
	exp.new_value = value;
	return exp;
      }
    finally
      {
        returnContinuation = saveContext;
      }
  }

  protected Expression walkTryExp (TryExp exp)
  {
    Expression saveContext = returnContinuation;
    try
      {
        if (exp.finally_clause != null)
          {
            returnContinuation = exp.try_clause;
          }
        exp.try_clause = exp.try_clause.walk(this);
        CatchClause catch_clause = exp.catch_clauses;
        while (exitValue == null && catch_clause != null)
          {
            if (exp.finally_clause != null)
              returnContinuation = catch_clause.body;
            catch_clause.body = catch_clause.body.walk(this);
            catch_clause = catch_clause.getNext();
          }
        Expression finally_clause = exp.finally_clause;
        if (finally_clause != null)
          {
            returnContinuation = finally_clause;
            exp.finally_clause = finally_clause.walk(this);
          }
        return exp;
      }
    finally
      {
        returnContinuation = saveContext;
      }
  }

  protected Expression walkSynchronizedExp (SynchronizedExp exp)
  {
    Expression saveContext = returnContinuation;
    try
      {
        returnContinuation = exp.object;
        exp.object = exp.object.walk(this);
        returnContinuation = exp.body;
        exp.body = exp.body.walk(this);
        return exp;
      }
    finally
      {
        returnContinuation = saveContext;
      }
  }
}
