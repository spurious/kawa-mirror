package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import kawa.lang.FString;
import java.lang.reflect.Modifier;

public class MakeInstance extends ProcedureN implements Inlineable
{
  public static MakeInstance make = new MakeInstance("make");

  public MakeInstance(String name)
  {
    super(name);
  }

  public Object applyN(Object[] args)
  {
    return applyN(this, args);
  }

  public static Object apply$V(Object[] args)
  {
    return applyN(make, args);
  }

  public static Object applyN(Procedure thisProc, Object[] args)
  {
    int len = args.length;
    Procedure.checkArgCount(thisProc, len);
    len -= 1;
    Object arg0 = args[0];
    String mname;
    MethodProc proc = ClassMethods.apply(thisProc, args[0], "<init>",
                                         null, null, 0, 0);

    Object[] rargs = new Object[len];
    System.arraycopy(args, 1, rargs, 0, len);

    Object vars = proc.getVarBuffer();
    RuntimeException ex = proc.match(vars, rargs);
    if (ex == null)
      return proc.applyV(vars);
    else if ((len & 1) == 0)
      {
        // Check if rargs is a set of (keyword,value)-pairs.
        for (int i = 0;  i < len;  i += 2)
          {
            if (! (rargs[i] instanceof Keyword))
              throw ex;
          }

        Object result = proc.apply0();
        for (int i = 0;  i < len;  i += 2)
          {
            Keyword key = (Keyword) rargs[i];
            Object arg = rargs[i+1];
            SlotSet.apply(result, key.getName(), arg);
          }
        return result;
      }
    throw ex;
  }

  public int numArgs() { return (-1 << 12) | 1; }


  private PrimProcedure cacheProc;
  private Expression[] cacheArgs;

  PrimProcedure getMethod(Expression[] args)
  {
    int nargs = args.length - 1;
    if (nargs < 0)
      return null;
    if (args == cacheArgs)
      return cacheProc;
    Expression arg0 = args[0];
    Type type = kawa.standard.Scheme.exp2Type(arg0);
    cacheArgs = args;
    if (type instanceof ClassType)
      {
        ClassType ctype = (ClassType) type;
        Procedure proc
          = ClassMethods.apply(this, type, "<init>", null, null, 0, 0);
        if (proc instanceof PrimProcedure)
          return (PrimProcedure) proc;
      }
    cacheProc = null;
    return null;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length - 1;
    if (nargs < 0)
      comp.error('e', "too few arguments to make");
    else
      {
        PrimProcedure proc = getMethod(args);
        System.err.println("INLINE make proc:"+proc);
        if (proc != null)
          {
            Expression[] margs = new Expression[nargs];
            System.arraycopy(args, 1, margs, 0, nargs);
            proc.compile(new ApplyExp(proc, margs), comp, target);
            return;
          }
      }
    ApplyExp.compile(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    if (args.length > 0)
      {
        Type type = kawa.standard.Scheme.exp2Type(args[0]);
        if (type != null)
          return type;
      }
    return Type.pointer_type;
  }
}
