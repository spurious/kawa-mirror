package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.Access;

/**
 * The Syntax transformer that re-writes the Kawa "location" primitive.
 * @author	Per Bothner
 */

public class location extends Syntax implements Printable
{
  static private Pattern pattern = new ListPat (2, 2);

  /** Assuming obj is in an lvalue context, re-write it. */
  public static Expression rewriteArg (Object obj, Translator tr)
  {
    while (obj instanceof Pair)
      {
	Pair pair = (Pair) obj;
	Object proc = pair.car;
	Object args = pair.cdr;
	Syntax syntax = tr.check_if_Syntax (proc);
	if (syntax == null)
	  {
	    int nargs = List.length(args);
	    Expression[] xargs = new Expression[nargs];
	    for (int i = 0; i < nargs; i++)
	      {
		pair = (Pair) args;
		xargs[i] = tr.rewrite(pair.car);
		args = pair.cdr;
	      }
	    return new ApplyExp(tr.rewrite(proc), xargs);
	  }
	if (syntax instanceof Macro)
	  obj = ((Macro) syntax).expand(pair.cdr, tr);
	else
	  return syntax.rewrite(pair.cdr, tr);
      }
    return tr.rewrite(obj);
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing argument to location");
    Pair pair = (Pair) obj;
    if (pair.cdr != List.Empty)
      return tr.syntaxError ("extra arguments to location");
    //    Expression arg = tr.rewrite(pair.car);
    Expression arg = rewriteArg(pair.car, tr);
    return location.rewrite(arg, tr);
  }

  public static Expression rewrite (Expression arg, Translator tr)
  {
    if (arg instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) arg;
	rexp.setDontDereference(true);
	Declaration binding = rexp.getBinding();
	if (binding != null)
	  {
	    binding.setIndirectBinding(true);
	    return rexp;
	  }
	else
	  {
	    gnu.bytecode.Method meth = Compilation.typeEnvironment
	      .addMethod("getCurrentBinding", Compilation.string1Arg,
			 Compilation.typeBinding,
			 Access.PUBLIC|Access.STATIC);
	    Expression proc = new QuoteExp(new PrimProcedure(meth));
	    Expression[] args = new Expression[1];
	    args[0] = new QuoteExp(rexp.getName());
	    return new ApplyExp(proc, args);
	  }
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
