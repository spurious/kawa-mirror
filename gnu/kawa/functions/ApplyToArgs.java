package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.ArrayGet;

/** Implement the standard Scheme function "apply".
 * This has been generalized so that the last (list argument)
 * can be any sequence, or any primitive array coercible to Object[]. */

public class ApplyToArgs extends ProcedureN
  implements CanInline
{
  // FIXME need matchN
  // FIXME need apply(CallContext).

  public ApplyToArgs (String name, Language language)
  {
    super(name);
    this.language = language;
  }

  Language language;

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length - 1;
    if (nargs >= 0)
      {
        Expression proc = args[0];
        args[0] = proc;
        Type ptype = proc.getType();
        if (ptype.isSubtype(Compilation.typeProcedure))
          {
            Expression[] rargs = new Expression[nargs];
            System.arraycopy(args, 1, rargs, 0, nargs);
            return ((InlineCalls) walker).walkApplyOnly(new ApplyExp(proc, rargs));
          }
        if (ptype.isSubtype(Compilation.typeType)
            || walker.getCompilation().getLanguage().getTypeFor(proc) != null)
          {
            return ((InlineCalls) walker).walkApplyOnly(new ApplyExp(Invoke.make, args));
          }
        if (ptype instanceof ArrayType)
          {
            Type elementType = ((ArrayType) ptype).getComponentType();
            return new ApplyExp(new ArrayGet(elementType), args);
          }
      }
    return exp;
  }

  public Object applyN (Object[] args) throws Throwable
  {
    Object proc = args[0];
    Object[] rargs = new Object[args.length-1];
    System.arraycopy(args, 1, rargs, 0, rargs.length);
    if (proc instanceof Procedure)
      {
        return ((Procedure) proc).applyN(rargs);
      }
    if (proc instanceof gnu.bytecode.Type)
      {
        return gnu.kawa.reflect.Invoke.make.applyN(args);
      }
    Class pclass = proc.getClass();
    if (pclass.isArray())
      {
        if (args.length != 2)
          throw new WrongArguments(this, args.length); // FIXME
        return java.lang.reflect.Array.get(proc, ((Number) rargs[0]).intValue());
      }
    System.err.println("ApplyToArgs applyN proc:"+proc);
    throw new WrongType(this, 0, proc, "procedure");
  }
}
