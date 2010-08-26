package gnu.expr;

/** Re-arranges ApplyExp where the function is a LetExp or BeginExp.
    Optimizes ((let (...) body) . args) to (let (...) (body . args)).
    Optimizes ((begin ... last) . args) to (begin ... (last . args)).
    This helps optimize Scheme "named let" (and some other forms)
    by making it more likely the application will be to a known procedure.
    This optimization has to be done after Declarations are bound. */

public class PushApply extends ExpVisitor<Expression,Void>
{
  public static void pushApply (Expression exp)
  {
    PushApply visitor = new PushApply();
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
    if (func instanceof LetExp
        && ! (func instanceof FluidLetExp)) // [APPLY-LET]
      {
	// Optimize ((let (...) body) . args) to (let (...) (body . args)).
	LetExp let = (LetExp) func;
	Expression body = let.body;
	let.body = exp;
	exp.func = body;
	return visit(let, ignored);
      }
    if (func instanceof BeginExp)  // [APPLY-BEGIN]
      {
	// Optimize ((begin ... last) . args) to (begin ... (last . args)).
	BeginExp begin = (BeginExp) func;
	Expression[] stmts = begin.exps;
	int last_index = begin.exps.length - 1;
	exp.func = stmts[last_index];
	stmts[last_index] = exp;
	return visit(begin, ignored);
      }
    exp.visitChildren(this, ignored);
    return exp;
  }
}
