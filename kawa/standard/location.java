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

public class location extends Syntax
{
  public static final location location = new location();
  static { location.setName("location"); }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing argument to location");
    Pair pair = (Pair) obj;
    if (pair.getCdr() != LList.Empty)
      return tr.syntaxError ("extra arguments to location");
    //    Expression arg = tr.rewrite(pair.getCar());
    Expression[] args = { location.rewrite(tr.rewrite(pair.getCar()), tr) };
    return Invoke.makeInvokeStatic(thisType, "makeLocationProc", args);
  }

  private static ClassType thisType = ClassType.make("kawa.standard.location");

  public static Expression rewrite (Expression arg, Translator tr)
  {
    if (arg instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) arg;
	rexp.setDontDereference(true);
	Declaration decl = rexp.getBinding();
	if (decl != null)
          {
            decl.maybeIndirectBinding(tr);
            decl = Declaration.followAliases(decl);
            decl.setCanRead(true);
            decl.setCanWrite(true);
          }
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
    int nargs = args.length;
    if (proc instanceof gnu.kawa.functions.ApplyToArgs
        && nargs > 0
        && args[0] instanceof Procedure) // FIXME
      {
        proc = (Procedure) args[0];
        if (proc instanceof LocationProc && nargs == 1)
          return ((LocationProc) proc).getLocation();
        Object[] rargs = new Object[nargs-1];
        System.arraycopy(args, 1, rargs, 0, rargs.length);
        return new ProcLocation(proc, rargs);
      }
    if (proc instanceof LocationProc && nargs == 0)
      return ((LocationProc) proc).getLocation();
    return new ProcLocation(proc, args);
  }

  public static Procedure
  makeLocationProc (Location loc)
  {
    return new LocationProc(loc);
  }
}
