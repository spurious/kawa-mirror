package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Implements the "constant-fold" transformer. */

public class constant_fold extends Syntax
{
  static Object checkConstant(Expression exp, Translator tr)
  {
    if (exp instanceof QuoteExp)
      return ((QuoteExp) exp).getValue();
    if (exp instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) exp;
	if (rexp.getBinding() == null)
	  {
	    try
	      {
		return ReferenceExp.lookup(rexp.getName());
	      }
	    catch (Exception ex)
	      {
		return null;
	      }
	  }
      }
    return null;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Expression exp = tr.rewrite(obj);
    if (! (exp instanceof ApplyExp))
      return exp;
    ApplyExp aexp = (ApplyExp) exp;
    Object func = checkConstant(aexp.getFunction(), tr);
    if (! (func instanceof Procedure))
      return exp;
    Expression[] args = aexp.getArgs();
    int i = args.length;
    Object[] vals = new Object[i];
    while (--i >= 0)
      {
	Object val = checkConstant(args[i], tr);
	if (val == null)
	  return exp;
	vals[i] = val;
      }
    try
      {
	return new QuoteExp(((Procedure) func).applyN(vals));
      }
    catch (Exception ex)
      {
	exp = tr.syntaxError("caught exception in constant-fold:");
	tr.syntaxError(ex.toString());
	return exp;
      }
  }
}
