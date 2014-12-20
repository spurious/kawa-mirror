package gnu.expr;
import gnu.bytecode.Type;
import java.util.HashMap;

/** A visitor that checks for tails-calls; also notes read/write/call accesses.
 * <p>
 * The main goal of this pass is to figure of which functions can and
 * should be inlined.  We inline a function where possible to avoid
 * closure allocation and related overheads, and also (more importantly)
 * to enable tail-call-optiomization: A tail-call can be implemented cheaply
 * and correctly using a {@code goto} bytecode instruction - but only for
 * functions inlined in the same JVM method.
 * <p>
 * We currently restrict inlining to cases when we can do so without
 * code duplication: When the function is only called once, not counting
 * tail-calls.  Because passing a "return link" is difficult, we require
 * that all calls to the function have the same "return continuation".
 * <p>
 * The extra visitor parameter is the {@code returnContinuation} - the
 * expression we "return to" - i.e. when done evaluating an expression, we're
 * also done with the {@code returnContinuation}.  Normally it is is same
 * {@code Expression} as we are visiting, but (for example) when visiting the
 * last expression of a {@code BeginExp} the  {@code returnContinuation}
 * is the same as that of the containing {@code BeginExp}.  We're in a
 * tail-context (in the sense of the Scheme reports) iff the current
 * {@code returnContinuation} is the {@code body} of the current
 * {@code LambdaExp}.
 * <p>
 * For each non-inlined function {@code f} we define {@code inlineSet(f)}
 * as the set of functions {@code g} such that {@code g.inlinedIn(f)}.
 * There are various requirements for a function {@code g} to be inlined;
 * for example we require that it have a fixed number of arguments.
 * Because of the no-duplication policy, all calls to {@code g} have
 * to be known, and all calls have to be from other inlined functions:
 * If {@code h} calls {@code g}, then {@code h==f || h.inlinedIn(f)}.
 * In addition all calls must have the same {@code returnContinuation}.
 * <p>
 * This analysis is done in two parts: First this {@code FindTailCalls}
 * visitor, and it is finished in {@code FindCapturedVars#checkInlineable}.
 * <p>
 * When this vistor is done, it has set the {@code returnContinuation},
 * {@code tailCallers}, and {@code inlineHome} fields of a {@code LambdaExp}.
 * If function {@code f} tail-calls {@code g}, then {@code f} is added
 * to the set of {@code g}'s {@code tailCaller}s.
 * <p>
 * If there is a non-tail-call to {@code g} then we try to set {@code g}'s
 * {@code returnContinuation} to the current (context)
 * {@code returnContinuation}, if the former hasn't been set yet;
 * otherwise we set it to the special {@code unknownContinuation} value.
 * We also construct {@code tailCallers} as the list of functions {@code h}
 * that make a tail-call to {@code g}.
 * At the end of this pass, if a function's {@code returnContinuation} is
 * is non-null and not {@code unknown} then it has a unique continuation,
 * and the function can potentially be inlined at the location of the
 * continuation.  However, that depends on if any tail-calls also have same
 * return-continuation; that analysis happens later in {@code checkInlineable}.
 * <p>
 * (In addition a {@code validate} method (executed during the earlier
 * {@code InlinedCalls} pass may pre-initialized the {@code returnContinuation}
 * {@code inlineHome} fields, but only for a {@code LambdaExp} that will be
 * inlined during code generation in a custom {@code compile} method.)
 */

public class FindTailCalls extends ExpExpVisitor<Expression>
{
  public static void findTailCalls (Expression exp, Compilation comp)
  {
    FindTailCalls visitor = new FindTailCalls();
    visitor.setContext(comp);
    visitor.visit(exp, exp);
  }

    public HashMap<Expression,Expression> savedReturnContinuations;

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
            // lead to memory leaks.  At least if interactive calls can
            // resolve to previously-compiled Declarations (as in XQuery).
            if (! binding.getFlag(Declaration.STATIC_SPECIFIED)
                && ! binding.inExternalModule(comp))
              {
                binding.addCaller(exp);
              }
            Compilation comp = getCompilation();
            Expression value = binding.getValue();
            if (value instanceof LambdaExp)
              lexp = (LambdaExp) value;
          }
      }
    else if (exp.func instanceof LambdaExp
             && ! (exp.func instanceof ClassExp))
      {
        lexp = (LambdaExp) exp.func;
        visitLambdaExp(lexp);
      }
    else if (exp.isAppendValues())
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

    protected Expression visitBlockExp(BlockExp exp, Expression returnContinuation) {
        if (savedReturnContinuations == null)
            savedReturnContinuations = new HashMap();
        savedReturnContinuations.put(exp, returnContinuation);
        exp.body = exp.body.visit(this, returnContinuation);
        if (exp.exitBody != null)
            exp.exitBody = exp.exitBody.visit(this, returnContinuation);
        return exp;
    }
    protected Expression visitExitExp(ExitExp exp, Expression returnContinuation) {
        BlockExp bl = exp.block;
        Expression res = exp.result;
        Expression retCont = bl.exitBody != null ? exp
            : savedReturnContinuations.get(bl);
        exp.result = res.visit(this, retCont);
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
    visitLetDecls(exp);
    exp.body = exp.body.visit(this, exp.body);
    postVisitDecls(exp);
    return exp;
  }

  void visitLetDecls (LetExp exp)
  {
    Declaration decl = exp.firstDecl();
    for (int i = 0;  decl != null;  i++, decl = decl.nextDecl())
      {
        Expression init = visitSetExp(decl, decl.getInitValue());
        // Optimize letrec-like forms.
        if (init == QuoteExp.undefined_exp)
          {
            Expression value = decl.getValue();
            if (value instanceof LambdaExp
                || (value != init && value instanceof QuoteExp))
              init = value;
          }
        decl.setInitValue(init);
      }
  }

  protected Expression visitLetExp (LetExp exp, Expression returnContinuation)
  {
    exp.clearCallList();
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

    protected Expression visitCaseExp(CaseExp exp, Expression returnContinuation) {
        exp.key = exp.key.visit(this, exp.key);
        for (int i = 0; i < exp.clauses.length; i++) {
            exp.clauses[i].exp = exp.clauses[i].exp.visit(this,
                    returnContinuation);
        }
        if (exp.elseClause != null)
            exp.elseClause.exp = exp.elseClause.exp.visit(this,
                    returnContinuation);
        return exp;
    }

  protected Expression visitLambdaExp (LambdaExp exp, Expression returnContinuation)
  {
    exp.clearCallList();
    visitLambdaExp(exp);
    return exp;
  }

  final void visitLambdaExp (LambdaExp exp)
  {
    LambdaExp parent = currentLambda;
    currentLambda = exp;
    try
      {
        visitDefaultArgs(exp, exp);
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
	  visitLambdaExp(child);
      }
    finally
      {
	currentLambda = parent;
      }

    return exp;
  }

  final Expression visitSetExp (Declaration decl, Expression value)
  {
    if (decl != null && decl.getValue() == value
	&& value instanceof LambdaExp && ! (value instanceof ClassExp)
        && ! decl.isPublic())
      {
	LambdaExp lexp = (LambdaExp) value; 
	visitLambdaExp(lexp);
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
