package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the Kawa "location" primitive.
 * @author	Per Bothner
 */

public class location extends Syntax implements Printable
{
  static private Pattern pattern = new ListPat (2, 2);

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing argument to location");
    Pair pair = (Pair) obj;
    if (pair.cdr != List.Empty)
      return tr.syntaxError ("extra arguments to location");
    Expression arg = tr.rewrite(pair.car);
    if (arg instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) arg;
	rexp.setDontDereference(true);
	Declaration binding = rexp.getBinding();
	if (binding != null)
	  binding.setIndirectBinding(true);
	return rexp;
      }
    if (arg instanceof ApplyExp)
      {
	ApplyExp aexp = (ApplyExp) arg;
	Expression func = tr.rewrite("%makeProcLocation");
	Expression[] args = new Expression[aexp.getArgs().length + 1];
	args[0] = aexp.getFunction();
	System.arraycopy(aexp.getArgs(), 0, args, 1, args.length-1);
	return new ApplyExp(func, args);
      }
    return tr.syntaxError("invalid argument to location");
  }
}
