package gnu.expr;

/** Re-arranges ApplyExp where the function is a LetExp or BeginExp.
    Optimizes ((let (...) body) . args) to (let (...) (body . args)).
    Optimizes ((begin ... last) . args) to (begin ... (last . args)).
    This helps optimize Scheme "named let" (and some other forms)
    by making it more likely the application will be to a known procedure.
    This optimization has to be done after Declarations are bound. */

public class PushApply extends ExpWalker
{
  public static void pushApply (Expression exp)
  {
    PushApply walker = new PushApply();
    exp.walk(walker);
    //or:  walter.walkExpression(exp);
  }

  protected Expression walkApplyExp(ApplyExp exp)
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
	return let.walk(this);
      }
    if (func instanceof BeginExp)  // [APPLY-BEGIN]
      {
	// Optimize ((begin ... last) . args) to (begin ... (last . args)).
	BeginExp begin = (BeginExp) func;
	Expression[] stmts = begin.exps;
	int last_index = begin.exps.length - 1;
	exp.func = stmts[last_index];
	stmts[last_index] = exp;
	return begin.walk(this);
      }
    exp.walkChildren(this);
    return exp;
  }
}
