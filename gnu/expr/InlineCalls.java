package gnu.expr;
import gnu.mapping.Procedure;

public class InlineCalls extends ExpFullWalker
{
  public static void inlineCalls (Expression exp)
  {
    InlineCalls walker = new InlineCalls();
    exp.walk(walker);
    //or:  walter.walkExpression(exp);
  }

  public Object walkApplyExp(ApplyExp exp)
  {
    super.walkApplyExp(exp);

    if (exp.func instanceof QuoteExp)
      {
	Object proc = ((QuoteExp) exp.func).getValue();
	if (proc instanceof CanInline)
	  {
	    return ((CanInline) proc).inline(exp);
	  }
      }
    if (exp.func instanceof ReferenceExp)
      {
        Declaration decl = ((ReferenceExp) exp.func).binding;
        //    System.err.println("Inline Call "+exp.func+" decl:"+decl);
        if (decl != null)
          {
            Object proc = decl.getValue();
            if (proc instanceof QuoteExp)
              proc = ((QuoteExp) proc).getValue();
            //            System.err.println("walk apply "+decl+" val:"+proc);
            if (proc instanceof CanInline)
              return ((CanInline) proc).inline(exp);
            /*
              if (proc instanceof Procedure)
              {
              PrimProcedure mproc
	      = PrimProcedure.getMethodFor((Procedure) proc, exp.args);
              if (mproc != null)
	      return new QuoteExp(mproc);
              }
            */
          }
      }
    return exp;
  }

  public Object walkSetExp (SetExp exp)
  {
    Declaration decl = exp.binding;
    boolean updateNeeded = false;
    if (decl != null)
      {
        Expression declValue = decl.getValue();
        if (declValue == exp.new_value)
          updateNeeded = true;
      }
    Object result = super.walkSetExp(exp);
    if (updateNeeded)
      {
        decl.value = exp.new_value;
        if (exp.new_value instanceof LambdaExp)
          ((LambdaExp) exp.new_value).nameDecl = decl;
      }
    return result;
  }
}
