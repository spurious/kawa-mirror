package gnu.expr;

/** Re-arranges ApplyExp where the function is a LetExp or BeginExp.
    Optimizes ((let (...) body) . args) to (let (...) (body . args)).
    Optimizes ((begin ... last) . args) to (begin ... (last . args)).
    This helps optimize Scheme "named let" (and some other forms)
    by making it more likely the application will be to a known procedure.
    Optimizes (if (let (...) body) e1 [e2])
      to (let (...) (if body e1 [e2])).
    Optimizes (if (begin ... last) e1 [e2])
      to (begin ... (if last e1 [e2]))
    These optimizations have to be done after Declarations are bound. */

public class PushApply extends ExpVisitor<Expression,Void>
{
  public static void pushApply (Expression exp, Compilation comp)
  {
    PushApply visitor = new PushApply();
    visitor.setContext(comp);
    visitor.visit(exp, null);
  }

  protected Expression update (Expression exp, Expression r)
  {
    return r;
  }

  protected Expression defaultValue(Expression r, Void ignored)
  {
    return r;
  }

  protected Expression visitApplyExp(ApplyExp exp, Void ignored)
  {
    Expression func = exp.func;
    boolean isApplyFunc = getCompilation().isApplyFunction(func)
      && exp.getArgCount() > 0;
    if (isApplyFunc)
      {
        func = exp.getArg(0);
      }
    if (func instanceof ReferenceExp)
      {
        Declaration fdecl = ((ReferenceExp) func).getBinding();
        if (fdecl != null && ! fdecl.hasUnknownValue())
          fdecl.addCaller(exp);
      }
    if (func instanceof LetExp
        && ! (func instanceof FluidLetExp)) // [APPLY-LET]
      {
	// Optimize ((let (...) body) . args) to (let (...) (body . args))
        // or (APPLY (let (...) body) . args) to (let (...) (APPLY body . args))
	LetExp let = (LetExp) func;
	Expression body = let.body;
	let.body = exp;
        if (isApplyFunc)
          exp.args[0] = body;
        else
          exp.func = body;
	return visit(let, ignored);
      }
    if (func instanceof BeginExp)  // [APPLY-BEGIN]
      {
	// Optimize ((begin ... last) . args) to (begin ... (last . args))
        // or (APPLY (begin ... last) . args) to (begin ... (APPLY last . args))
	BeginExp begin = (BeginExp) func;
	Expression[] stmts = begin.exps;
	int last_index = begin.exps.length - 1;
        if (isApplyFunc)
          exp.args[0] = stmts[last_index];
        else
          exp.func = stmts[last_index];
        stmts[last_index] = exp;
	return visit(begin, ignored);
      }
    exp.visitChildren(this, ignored);
    return exp;
  }

    protected Expression visitIfExp(IfExp exp, Void ignored) {
        Expression test = exp.test;
        if (test instanceof LetExp
            && ! (test instanceof FluidLetExp)) { // [IF-LET] 
            // Optimize (if (let (...) body) e1 [e2])
            // to (let (...) (if body e1 [e2]))
            LetExp let = (LetExp) test;
            Expression body = let.body;
            let.body = exp;
            exp.test = body;
            return visit(let, ignored);
        }
        else if (test instanceof BeginExp) { // [IF-BEGIN]
            // Optimize (if (begin ... last) e1 [e2])
            // to (begin ... (if last e1 [e2])).
            BeginExp begin = (BeginExp) test;
            Expression[] stmts = begin.exps;
            int last_index = begin.exps.length - 1;
            exp.test = stmts[last_index];
            stmts[last_index] = exp;
            return visit(begin, ignored);
        }
        else
            return super.visitIfExp(exp, ignored);
    }

  protected Expression visitReferenceExp (ReferenceExp exp, Void ignored)
  {
    Declaration decl = exp.getBinding();
    if (decl != null)
      {
        decl.numReferences++;
        // Figure out the innerLambda, which is the LambdaExp (if any)
        // between the declaration and the current context.
        if (decl.context instanceof LetExp)
          {
            LambdaExp innerLambda = getCurrentLambda();
            for (ScopeExp sc = innerLambda; sc != null; sc = sc.outer)
              {
                if (sc == decl.context)
                  {
                    // Chain on to innerLambda.siblingReferences list.
                    exp.siblingReferencesNext = innerLambda.siblingReferences;
                    innerLambda.siblingReferences = exp;
                    break;
                  }
                if (sc instanceof LambdaExp)
                  innerLambda = (LambdaExp) sc;
              }
          }
      }
    return super.visitReferenceExp(exp, ignored);
  }

  protected Expression visitClassExp (ClassExp exp, Void ignored)
  {
    // Allocate class fields and methods.  Ideally, setting field and method
    // types should be deferred until InlineClass, when we do type inferencing.
    // But doing it just before InlineCalls works tolerably enough for now.
    exp.declareParts(getCompilation());
    return visitLambdaExp(exp, ignored);
  }
}
