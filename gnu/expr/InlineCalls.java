package gnu.expr;
import gnu.mapping.Procedure;

public class InlineCalls extends ExpWalker
{
  public static void inlineCalls (Expression exp)
  {
    InlineCalls walker = new InlineCalls();
    exp.walk(walker);
    //or:  walter.walkExpression(exp);
  }

  protected Expression walkApplyExp(ApplyExp exp)
  {
    super.walkApplyExp(exp);
    LambdaExp lambda = null;
    if (exp.func instanceof LambdaExp)
      lambda = (LambdaExp) exp.func;
    if (exp.func instanceof QuoteExp)
      {
	Object proc = ((QuoteExp) exp.func).getValue();
	if (proc instanceof CanInline)
	  {
	    return ((CanInline) proc).inline(exp, this);
	  }
      }
    if (exp.func instanceof ReferenceExp)
      {
        Declaration decl = ((ReferenceExp) exp.func).binding;
        if (decl != null && ! decl.getFlag(Declaration.IS_UNKNOWN))
	  {
            Object proc = decl.getValue();
	    if (proc instanceof LambdaExp) 
	      lambda = (LambdaExp) proc;
            if (proc instanceof QuoteExp)
              proc = ((QuoteExp) proc).getValue();
            if (proc instanceof CanInline)
              return ((CanInline) proc).inline(exp, this);
            // if (proc instanceof Procedure)
	    // {
	    //   PrimProcedure mproc
	    //    = PrimProcedure.getMethodFor((Procedure) proc, exp.args);
	    //   if (mproc != null)
	    //     return new QuoteExp(mproc);
	    // }
	  }
      }
    if (lambda != null)
      {
	int args_length = exp.args.length;
        String msg = null;
	if (args_length < lambda.min_args)
          msg = "too few args for ";
	else if (lambda.max_args >= 0 && args_length > lambda.max_args)
          msg = "too many args "+args_length+" for ";
	// FIXME make error message
	if (msg != null)
	  System.err.println("bad call: "+msg+lambda.getName());
      }
    return exp;
  }

  protected Expression walkSetExp (SetExp exp)
  {
    Declaration decl = exp.binding;
    boolean updateNeeded = false;
    if (decl != null)
      {
        Expression declValue = decl.getValue();
        if (declValue == exp.new_value)
          updateNeeded = true;
      }
    exp.walkChildren(this);
    if (updateNeeded)
      {
        decl.value = exp.new_value;
        if (exp.new_value instanceof LambdaExp)
          ((LambdaExp) exp.new_value).nameDecl = decl;
      }
    return exp;
  }
}
