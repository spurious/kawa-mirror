package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.expr.*;

public class MakeProcedure extends ProcedureN implements CanInline
{
  public static final MakeProcedure makeProcedure = new MakeProcedure();
  static { makeProcedure.setName("make-procedure"); }

  public static GenericProc makeProcedure$V (Object[] args)
  {
    return GenericProc.make(args);
  }

  public Object applyN(Object[] args)
  {
    return GenericProc.make(args);
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Expression[] args = exp.getArgs();
    int alen = args.length;
    Expression method = null;
    int countMethods = 0;
    String name = null;
    for (int i = 0;  i < alen;  i++)
      {
	Expression arg = args[i];
        Object key;
	if (arg instanceof QuoteExp
            && (key = ((QuoteExp) arg).getValue()) instanceof Keyword)
	  {
	    String keyword = ((Keyword) key).getName();
	    Expression next = args[++i];
	    if (keyword == "name")
              {
                if (next instanceof QuoteExp)
                  name = ((QuoteExp) next).getValue().toString();
              }
	    else if (keyword == "method")
              {
                countMethods++;
                method = next;
              }
	    else
	      ; // result.setProperty(keyword, value);
	  }
	else
          {
            countMethods++;
            method = arg;
          }
      }
    if (countMethods == 1 && method instanceof LambdaExp)
      {
        LambdaExp proc = (LambdaExp) method;
        for (int i = 0;  i < alen;  i++)
          {
            Expression arg = args[i];
            Object key;
            if (arg instanceof QuoteExp
                && (key = ((QuoteExp) arg).getValue()) instanceof Keyword)
              {
                String keyword = ((Keyword) key).getName();
                Expression next = args[++i];
                if (keyword == "name")
                  proc.setName(name);
                else if (keyword == "method")
                  ;
                else
                  proc.setProperty(Namespace.EmptyNamespace.getSymbol(keyword), next);
              }
          }
        return method;
      }
    return exp;
  }
}
