package gnu.expr;
import gnu.mapping.*;
import gnu.text.*;

public class InlineCalls extends ExpWalker
{
  Compilation comp;

  public static void inlineCalls (Expression exp, Compilation comp)
  {
    InlineCalls walker = new InlineCalls();
    walker.comp = comp;
    walker.messages = comp.getMessages();
    walker.walk(exp);
  }

  protected Expression walkApplyExp(ApplyExp exp)
  {
    super.walkApplyExp(exp);
    LambdaExp lambda = null;
    int nargs = exp.getArgCount();
    if (exp.func instanceof LambdaExp)
      lambda = (LambdaExp) exp.func;
    Expression func = exp.func;
    Declaration decl = null;
    if (func instanceof ReferenceExp)
      {
        decl = ((ReferenceExp) func).binding;
        if (decl != null && ! decl.getFlag(Declaration.IS_UNKNOWN))
	  {
            func = decl.getValue();
	    if (func instanceof LambdaExp) 
	      lambda = (LambdaExp) func;
	  }
      }
    if (func instanceof QuoteExp)
      {
	Object fval = ((QuoteExp) func).getValue();
	if (! (fval instanceof Procedure))
	  return noteError(decl == null ? "called value is not a procedure"
			   : ("calling " + decl.getName()
			      + " which is not a procedure"));
	Procedure proc = (Procedure) fval;
	String msg = WrongArguments.checkArgCount(proc, nargs);
	if (msg != null)
	  return noteError(msg);
	if (proc instanceof CanInline)
	  return ((CanInline) proc).inline(exp, this);
	PrimProcedure mproc
	  = PrimProcedure.getMethodFor(proc, decl, exp.args,
				       comp.getInterpreter());
	if (mproc != null)
	  {
	    if (mproc.getStaticFlag())
	      return new ApplyExp(mproc, exp.args);
	    Expression[] margs = new Expression[1 + nargs];
	    System.arraycopy(exp.getArgs(), 0, margs, 1, nargs);
	    margs[0] = new ReferenceExp(decl.base);
	    return new ApplyExp(mproc, margs);
	  }
      }
    if (lambda != null)
      {
	int args_length = exp.args.length;
	String msg = WrongArguments.checkArgCount(lambda.getName(),
						  lambda.min_args,
						  lambda.max_args,
						  args_length);
	if (msg != null)
	  return noteError(msg);
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
