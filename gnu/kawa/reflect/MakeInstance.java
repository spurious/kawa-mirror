package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import kawa.lang.FString;

public class MakeInstance extends ProcedureN
{
  public Object applyN(Object[] args)
  {
    int len = args.length;
    Procedure.checkArgCount(this, len);
    len -= 1;
    Object arg0 = args[0];
    ClassType dtype;
    String mname;
    if (arg0 instanceof Class)
      arg0 = Type.make((Class) arg0);
    if (arg0 instanceof ClassType)
      dtype = (ClassType) arg0;
    else if (arg0 instanceof String || arg0 instanceof FString)
      dtype = ClassType.make(arg0.toString());
    else
      throw new WrongType(this, 0, null);

    MethodProc proc = ClassMethods.apply(dtype, "<init>", null, null, 0, 0);

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

        Object result;
        try
          {
            result = dtype.getReflectClass().newInstance();
          }
        catch (InstantiationException e)
          {
            throw new RuntimeException(e.toString());
          }
        catch (IllegalAccessException e)
          {
            throw new RuntimeException(e.toString());
          }
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
}
