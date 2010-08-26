package gnu.expr;
import gnu.bytecode.Type;

/** A visitor that checks for tails-calls; also notes read/write/call accesses.
 *
 * Does setTailCall on ApplyExp's that are tail-calls.
 * Also setCanRead, setCanCall on Declarations
 * and setCanRead, setCanCall on LambdaExp when appropriate.
 * (setCanWrite on Declarations needs to be set before this.)
 * Note the final part of deciding inlineability has to be done after
 * FindTailCalls finishes (or at least after we've visited all possible
 * callers), so it is deferred to FindCapturedvars.visitLambdaExp.
 *
 * The extra parameter is the {@code returnContinuation} - the expression we
 * "return to" - i.e. when done eveluating an expression, we're also done
 * with the {@code returnContinuation}.  Normally it is is same
 * {@code Expression} as we are visiting, but (for example) when visiting the
 * last expression of a {@code BeginExp} the  {@code returnContinuation}
 * is the same as that of the containing {@code BeginExp}.
 */

public class FindTailCalls extends ExpExpVisitor<Expression>
{
  public static void findTailCalls (Expression exp, Compilation comp)
  {
    FindTailCalls visitor = new FindTailCalls();
    visitor.setContext(comp);
    visitor.visit(exp, exp);
  }

  protected Expression visitExpression (Expression exp, Expression returnContinuation)
  {
    return super.visitExpression(exp, exp);
  }

  public Expression[] visitExps (Expression[] exps)
  {
    int n = exps.length;
    for (int i = 0;  i < n;  i++)
      {
        Expression expi = exps[i];
        exps[i] = visit(expi, expi);
      }
    return exps;
  }

  protected Expression visitApplyExp (ApplyExp exp, Expression returnContinuation)
  {
    boolean inTailContext = returnContinuation == currentLambda.body;
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
        visitLambdaExp(lexp, false);
        lexp.setCanCall(true);
      }
    else if (exp.func instanceof QuoteExp
             && (((QuoteExp) exp.func).getValue()
                 == gnu.kawa.functions.AppendValues.appendValues))
      isAppendValues = true;
    else
      {
        exp.func = visitExpression(exp.func, exp.func);
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
    /* This conflates the concepts of a tail-call with being able to
       optimize away append-values.  FIXME
    if (isAppendValues
        && currentLambda.getCallConvention() >= Compilation.CALL_WITH_CONSUMER)
     {
       Expression[] args = exp.args;
       int nargs = args.length;
       for (int i = 0;  i < nargs;  i++)
         {
           args[i] = visit(args[i], null);
         }
      }
      else*/
    exp.args = visitExps(exp.args);
    return exp;
  }

  protected Expression visitBlockExp (BlockExp exp, Expression returnContinuation)
  {
    exp.body = exp.body.visit(this, returnContinuation);
    if (exp.exitBody != null)
      exp.exitBody = exp.exitBody.visit(this, exp.exitBody);
    return exp;
  }

  protected Expression visitBeginExp (BeginExp exp, Expression returnContinuation)
  {
    int n = exp.length - 1;
    for (int i = 0;  i <= n;  i++)
      exp.exps[i] = exp.exps[i].visit(this, i == n ? returnContinuation : exp.exps[i]);
    return exp;
  }

  protected Expression visitFluidLetExp (FluidLetExp exp, Expression returnContinuation)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null; decl = decl.nextDecl())
      {
        decl.setCanRead(true);
        if (decl.base != null)
          decl.base.setCanRead(true);
      }
    visitLetDecls(exp);
    exp.body = exp.body.visit(this, exp.body);
    postVisitDecls(exp);
    return exp;
  }

  void visitLetDecls (LetExp exp)
  {
    Declaration decl = exp.firstDecl();
    int n = exp.inits.length; 
    for (int i = 0;  i < n;  i++, decl = decl.nextDecl())
      {
        Expression init = visitSetExp(decl, exp.inits[i]);
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

  protected Expression visitLetExp (LetExp exp, Expression returnContinuation)
  {
    visitLetDecls(exp);
    exp.body = exp.body.visit(this, returnContinuation);
    postVisitDecls(exp);
    return exp;
  }

  public void postVisitDecls (ScopeExp exp)
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

  protected Expression visitIfExp (IfExp exp, Expression returnContinuation)
  {
    exp.test = exp.test.visit(this, exp.test);
    exp.then_clause = exp.then_clause.visit(this, returnContinuation);
    Expression else_clause = exp.else_clause;
    if (else_clause != null)
      exp.else_clause = else_clause.visit(this, returnContinuation);
    return exp;
  }

  protected Expression visitLambdaExp (LambdaExp exp, Expression returnContinuation)
  {
    visitLambdaExp (exp, true);
    return exp;
  }

  final void visitLambdaExp (LambdaExp exp, boolean canRead)
  {
    LambdaExp parent = currentLambda;
    currentLambda = exp;
    if (canRead)
      exp.setCanRead(true);
    try
      {
	if (exp.defaultArgs != null)
	  exp.defaultArgs = visitExps(exp.defaultArgs);
	if (exitValue == null && exp.body != null)
	  exp.body = exp.body.visit(this, exp.getInlineOnly() ? exp : exp.body);
      }
    finally
      {
	currentLambda = parent;
      }

    postVisitDecls(exp);
  }


  // Map LambdaExp to ApplyExp[], which is the set of non-self tails
  // calls that call the key.
  // Hashtable applications = new Hashtable();

  protected Expression visitClassExp (ClassExp exp, Expression returnContinuation)
  {
    LambdaExp parent = currentLambda;
    currentLambda = exp;
    try
      {
	for (LambdaExp child = exp.firstChild;
	     child != null && exitValue == null;  child = child.nextSibling)
	  visitLambdaExp(child, false);
      }
    finally
      {
	currentLambda = parent;
      }

    return exp;
  }

  protected Expression visitReferenceExp (ReferenceExp exp, Expression returnContinuation)
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

  final Expression visitSetExp (Declaration decl, Expression value)
  {
    if (decl != null && decl.getValue() == value
	&& value instanceof LambdaExp && ! (value instanceof ClassExp)
        && ! decl.isPublic())
      {
	LambdaExp lexp = (LambdaExp) value; 
	visitLambdaExp(lexp, false);
	return lexp;
      }
    else
      return value.visit(this, value);
  }

  protected Expression visitSetExp (SetExp exp, Expression returnContinuation)
  {
    Declaration decl = exp.binding;
    if (decl != null && decl.isAlias())
      {
        if (exp.isDefining())
          {
            exp.new_value = exp.new_value.visit(this, exp.new_value);
            return exp;
          }
        decl = Declaration.followAliases(decl);
      }
    Declaration ctx = exp.contextDecl();
    if (ctx != null)
      ctx.setCanRead(true);
    Expression value = visitSetExp(decl, exp.new_value);
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

  protected Expression visitTryExp (TryExp exp, Expression returnContinuation)
  {
    Expression tryContinuation
      = exp.finally_clause == null ? returnContinuation : exp.try_clause;
    exp.try_clause = exp.try_clause.visit(this, tryContinuation);
    CatchClause catch_clause = exp.catch_clauses;
    while (exitValue == null && catch_clause != null)
      {
        Expression clauseContinuation
          = exp.finally_clause == null ? returnContinuation : catch_clause.body;
        catch_clause.body = catch_clause.body.visit(this, clauseContinuation);
        catch_clause = catch_clause.getNext();
      }
    Expression finally_clause = exp.finally_clause;
    if (finally_clause != null)
      exp.finally_clause = finally_clause.visit(this, finally_clause);
    return exp;
  }

  protected Expression visitSynchronizedExp (SynchronizedExp exp, Expression returnContinuation)
  {
    exp.object = exp.object.visit(this, exp.object);
    exp.body = exp.body.visit(this, exp.body);
    return exp;
  }
}
