package gnu.kawa.reflect;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import java.lang.reflect.Modifier;

public class InvokeStatic extends ProcedureN implements Inlineable
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    Procedure.checkArgCount(this, len);
    len -= 2;
    Procedure proc = ClassMethods.apply(this, args[0], args[1], null, null,
                                        Modifier.STATIC, Modifier.STATIC);
    Object[] rargs = new Object[len];
    System.arraycopy(args, 2, rargs, 0, len);
    return proc.applyN(rargs);
  }

  public int numArgs() { return (-1 << 12) | 2; }

  private PrimProcedure cacheProc;
  private Expression[] cacheArgs;

  PrimProcedure getMethod(Expression[] args)
  {
    int nargs = args.length - 2;
    if (nargs < 0)
      return null;
    if (args == cacheArgs)
      return cacheProc;
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    Type type = kawa.standard.Scheme.exp2Type(arg0);
    cacheArgs = args;
    if (type instanceof ClassType && arg1 instanceof QuoteExp)
      {
        ClassType ctype = (ClassType) type;
        Object val = ((QuoteExp) arg1).getValue();
        if (val instanceof kawa.lang.FString || val instanceof String)
          {
            String mname = val.toString();
            Procedure proc
              = ClassMethods.apply(this, type, val, null, null,
                                   Modifier.STATIC, Modifier.STATIC);
            if (proc instanceof PrimProcedure)
              return (PrimProcedure) proc;
          }
      }
    cacheProc = null;
    return null;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length - 2;
    if (nargs < 0)
      comp.error('e', "too few arguments to invoke-static");
    else
      {
        PrimProcedure proc = getMethod(args);
        if (proc != null)
          {
            Expression[] margs = new Expression[nargs];
            System.arraycopy(args, 2, margs, 0, nargs);
            proc.compile(new ApplyExp(proc, margs), comp, target);
            return;
          }
      }
    ApplyExp.compile(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    PrimProcedure proc = getMethod(args);
    if (proc != null)
      return proc.getReturnType(args);
    return Type.pointer_type;
  }
}
