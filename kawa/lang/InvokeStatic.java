package kawa.lang;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class InvokeStatic extends ProcedureN implements Inlineable
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    Procedure.checkArgCount(this, len);
    ClassType type;
    Object t = args[0];
    if (t instanceof ClassType)
      type = (ClassType) t;
    else if (t instanceof String || t instanceof kawa.lang.FString)
      type = ClassType.make(t.toString());
    else
      throw WrongType.make(null, this, 0);
      
    t = args[1];
    if (! (t instanceof String || t instanceof kawa.lang.FString))
      throw WrongType.make(null, this, 1);
    String mname = t.toString();

    Class clas = type.getReflectClass();
    if (clas == null)
      throw new RuntimeException("no such class: "+type.getName());
    int nMethods = type.getMethodCount();
    if (nMethods == 0)
      {
        type.addMethods(clas);
        nMethods = type.getMethodCount();
      }
    int nargs = len - 2;
    Type[] paramTypes = new Type[nargs];
    for (int i = nargs;  --i >= 0; )
      paramTypes[i] = Type.make(args[i + 2].getClass());
    Method[] matches = type.getMatchingMethods(mname, paramTypes, Access.STATIC);
    if (matches.length == 1)
      {
        PrimProcedure proc = new PrimProcedure(matches[0]);
        Object[] margs = new Object[nargs];
        System.arraycopy(args, 2, margs, 0, nargs);
        return proc.applyN(margs);
      }
    else
      throw new RuntimeException((matches.length == 0 ? "no static method "
                                  : "ambiguous static method (ignoring parameter types) ")
                                 +type.getName()+"."+mname);
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
            int nMethods = ctype.getMethodCount();
            Type[] paramTypes = new Type[nargs];
            for (int i = nargs;  --i >= 0; )
              paramTypes[i] = args[i+2].getType();

            if (nMethods == 0)
              {
                Class cclass = ctype.getReflectClass();
                if (cclass != null)
                  ctype.addMethods(cclass);
              }
            Method[] matches = ctype.getMatchingMethods(mname, paramTypes, Access.STATIC);
            if (matches.length == 1)
              return cacheProc = new PrimProcedure(matches[0]);
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
