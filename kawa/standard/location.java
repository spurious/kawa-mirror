package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.mapping.Location;  // As opposed to gnu.bytecode.Location.
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.Invoke;

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
    if (pair.cdr != LList.Empty)
      return tr.syntaxError ("extra arguments to location");
    //    Expression arg = tr.rewrite(pair.car);
    Expression[] args = { location.rewrite(tr.rewrite(pair.car), tr) };
    return Invoke.makeInvokeStatic(thisType, "makeLocationProc", args);
  }

  private static ClassType thisType = ClassType.make("kawa.standard.location");

  public static Expression rewrite (Expression arg, Translator tr)
  {
    if (arg instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) arg;
	rexp.setDontDereference(true);
	Declaration binding = rexp.getBinding();
	if (binding != null && binding.isLexical())
	  binding.setIndirectBinding(true);
	return rexp;
      }
    if (arg instanceof ApplyExp)
      {
	ApplyExp aexp = (ApplyExp) arg;
	Expression[] args = new Expression[aexp.getArgs().length + 1];
	args[0] = aexp.getFunction();
	System.arraycopy(aexp.getArgs(), 0, args, 1, args.length-1);
	return Invoke.makeInvokeStatic(thisType, "makeProcLocation", args);
      }
    return tr.syntaxError("invalid argument to location");
  }

  public static Location
  makeProcLocation$V (Procedure proc, Object[] args)
  {
    if (proc instanceof LocationProc && args.length == 0)
      return ((LocationProc) proc).getLocation();
    return new ProcLocation(proc, args);
  }

  public static Procedure
  makeLocationProc (Location loc)
  {
    return new LocationProc(loc);
  }
}
