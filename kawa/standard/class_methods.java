package kawa.standard;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;
import kawa.lang.FString;

public class class_methods extends ProcedureN
{
  public int numArgs() { return 2 | (2 << 12); } // For now.

  public Object applyN (Object[] args)
  {
    return apply(this, args[0], args[1], null, null, 0, 0);
  }

  public static Procedure apply(Procedure thisProc,
                                Object arg0, Object arg1,
                                Type rtype, Type[] atypes,
                                int modifiers, int modmask)
  {
    ClassType dtype;
    String mname;
    if (arg0 instanceof Class)
      arg0 = Type.make((Class) arg0);
    if (arg0 instanceof ClassType)
      dtype = (ClassType) arg0;
    else if (arg0 instanceof String || arg1 instanceof FString)
      dtype = ClassType.make(arg0.toString());
    else
      throw new WrongType(thisProc, 0, null);
    if (arg1 instanceof String || arg1 instanceof FString)
      mname = arg1.toString();
    else
      throw new WrongType(thisProc, 1, null);
    mname = Compilation.mangleName(mname);
    return apply(dtype, mname, null, null, 0, 0);
  }

  public static Procedure apply(ClassType dtype, String mname,
                                Type rtype, Type[] atypes,
                                int modifiers, int modmask)
  {
    kawa.lang.Interpreter interpreter = Scheme.getInstance();
    Class dclass = dtype.getReflectClass();
    if (dclass == null)
      throw new RuntimeException("no such class: "+dtype.getName());
    java.lang.reflect.Method[] methods = getMethods(dclass, mname,
                                                    modifiers, modmask);
    GenericProc gproc = null;
    PrimProcedure pproc = null;
    for (int i = 0;  i < methods.length;  i++)
      {
        java.lang.reflect.Method rmethod = methods[i];
        Class[] rparams = rmethod.getParameterTypes();
        int j = rparams.length;
        Type[] params = new Type[j];
        while (--j >= 0)
          params[j] = interpreter.getTypeFor(rparams[j]);
        gnu.bytecode.Method method
          = dtype.addMethod(mname, rmethod.getModifiers(), params,
                            interpreter.getTypeFor(rmethod.getReturnType()));
        if (pproc != null && gproc == null)
          {
            gproc = new GenericProc();
            gproc.add(pproc);
          }
        pproc = new PrimProcedure(method);
        if (gproc != null)
          gproc.add(pproc);
      }
    if (gproc != null)
      {
        gproc.setName(dtype.getName()+"."+mname);
        return gproc;
      }
    if (pproc != null)
      return pproc;
    throw new RuntimeException("no method named `"+mname+"' in "
                               +dtype.getName());
  }

  /** Re-order method array in place so one with selected name are first.
   * @return the number of methods whose names match. */
  public static int selectMethods(java.lang.reflect.Method[] methods,
                                  String mname, int modifiers, int modmask)
  {
    int n = methods.length;
    int num = 0;
    for (int i = 0;  i < n;  i++)
      {
        java.lang.reflect.Method method = methods[i];
        if (! mname.equals(method.getName()))
          continue;
        int mods = method.getModifiers();
        if ((mods & modmask) != modifiers)
          continue;
        methods[i] = methods[num];
        methods[num] = method;
        num++;
      }
    return num;
  }

  public static java.lang.reflect.Method[]
  getMethods(Class clas, String mname, int modifiers, int modmask)
  {
    java.lang.reflect.Method[] methods = clas.getMethods();
    int nmethods = selectMethods(methods, mname, modifiers, modmask);
    java.lang.reflect.Method[] matching
    = new java.lang.reflect.Method[nmethods];
    System.arraycopy(methods, 0, matching, 0, nmethods);
    return matching;
  }
}
