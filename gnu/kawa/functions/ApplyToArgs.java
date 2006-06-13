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
  public int match1 (Object arg1, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match0(ctx);
    else
      return super.match1(arg1, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match1(arg2, ctx);
    else
      return super.match2(arg1, arg2, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match2(arg2, arg3, ctx);
    else
      return super.match3(arg1, arg2, arg3, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
                     CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match3(arg2, arg3, arg4, ctx);
    else
      return super.match4(arg1, arg2, arg3, arg4, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    int n = args.length;
    if (n > 0 && args[0] instanceof Procedure)
      {
        Procedure proc = (Procedure) args[0];
        switch (n)
          {
          case 1:
            return proc.match0(ctx);
          case 2:
            return proc.match1(args[1], ctx);
          case 3:
            return proc.match2(args[1], args[2], ctx);
          case 4:
            return proc.match3(args[1], args[2], args[3], ctx);
          case 5:
            return proc.match4(args[1], args[2], args[3], args[4], ctx);
          default:
            Object[] xargs = new Object[n-1];
            System.arraycopy(args, 1, xargs, 0, n-1);
            return proc.matchN(xargs, ctx);
          }
      }
    return super.matchN(args, ctx);
  }

  public void check1 (Object arg1, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check0(ctx);
    else
      super.check1(arg1, ctx);
  }

  public void check2 (Object arg1, Object arg2, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check1(arg2, ctx);
    else
      super.check2(arg1, arg2, ctx);
  }

  public void check3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check2(arg2, arg3, ctx);
    else
      super.check3(arg1, arg2, arg3, ctx);
  }

  public void check4 (Object arg1, Object arg2, Object arg3, Object arg4,
                     CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check3(arg2, arg3, arg4, ctx);
    else
      super.check4(arg1, arg2, arg3, arg4, ctx);
  }

  public void checkN (Object[] args, CallContext ctx)
  {
    int code = matchN(args, ctx);
    if (code != 0)
      {
        Procedure proc = this;
        if (args.length > 0 && args[0] instanceof Procedure)
          {
            proc = (Procedure) args[0];
            Object[] xargs = new Object[args.length-1];
            System.arraycopy(args, 1, xargs, 0, xargs.length);
            args = xargs;
          }
	throw MethodProc.matchFailAsException(code, proc, args);
      }
  }

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
        ApplyExp result;
        Compilation comp = walker.getCompilation();
        Language language = comp.getLanguage();
        // This might be more cleanly handled at the type specifier. FIXME
        if (Invoke.checkKnownClass(ptype, comp) < 0)
          return exp;
        ClassType ctype;
        if (ptype.isSubtype(Compilation.typeProcedure))
          {
            Expression[] rargs = new Expression[nargs];
            System.arraycopy(args, 1, rargs, 0, nargs);
            result = new ApplyExp(proc, rargs);
          }
        else if (ptype.isSubtype(Compilation.typeType)
                 || language.getTypeFor(proc,false) != null)
          {
            result = new ApplyExp(Invoke.make, args);
          }
        else if (ptype instanceof ArrayType)
          {
            Type elementType = ((ArrayType) ptype).getComponentType();
            result = new ApplyExp(new ArrayGet(elementType), args);
          }
        else if (ptype instanceof ClassType
                 && (ctype = (ClassType) ptype).isSubclass(typeList)
                 && nargs == 1)
          {
            // We search for a "get(int)" method, rather than just using
            // typeList.getDeclaredMethod("get", 1) to see if we make a
            // a virtual call rather than an interface call.
            Method get = ctype.getMethod("get", new Type[] { Type.int_type });
            result = new ApplyExp(get, args);
          }
        else
          return exp;
        result.setLine(exp);
        return ((InlineCalls) walker).walkApplyOnly(result);
      }
    return exp;
  }

  static final ClassType typeList
  /* #ifdef JAVA2 */
  = ClassType.make("java.util.List");
  /* #else */
  // = ClassType.make("gnu.lists.Sequence");
  /* #endif */

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
    if (proc instanceof
        /* #ifdef JAVA2 */
        java.util.List
        /* #else */
        // gnu.lists.Sequence
        /* #endif */
        )
      {
        if (args.length != 2)
          throw new WrongArguments(this, args.length); // FIXME
        int index = ((Number) rargs[0]).intValue();
        /* #ifdef JAVA2 */
        return ((java.util.List) proc).get(index);
        /* #else */
        // return ((gnu.lists.Sequence) proc).get(index);
        /* #endif */
      }
    Class pclass = proc.getClass();
    if (pclass.isArray())
      {
        if (args.length != 2)
          throw new WrongArguments(this, args.length); // FIXME
        return java.lang.reflect.Array.get(proc, ((Number) rargs[0]).intValue());
      }
    throw new WrongType(this, 0, proc, "procedure");
  }
}
