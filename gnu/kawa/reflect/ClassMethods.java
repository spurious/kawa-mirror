package gnu.kawa.reflect;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;
import kawa.lang.FString;

public class ClassMethods extends ProcedureN
{
  public int numArgs() { return 2 | (2 << 12); } // For now.

  public Object applyN (Object[] args)
  {
    return apply(this, args[0], args[1], null, null, 0, 0);
  }

  public static MethodProc apply(Procedure thisProc,
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
    else if (arg0 instanceof String || arg0 instanceof FString)
      dtype = ClassType.make(arg0.toString());
    else
      throw new WrongType(thisProc, 0, null);
    if (arg1 instanceof String || arg1 instanceof FString)
      mname = arg1.toString();
    else
      throw new WrongType(thisProc, 1, null);
    if (! ("<init>".equals(mname)))
      mname = Compilation.mangleName(mname);
    return apply(dtype, mname, rtype, atypes, modifiers, modmask);
  }

  public static MethodProc apply(ClassType dtype, String mname,
                                 Type rtype, Type[] atypes,
                                 int modifiers, int modmask)
  {
    kawa.lang.Interpreter interpreter = kawa.standard.Scheme.getInstance();
    Class dclass = dtype.getReflectClass();
    if (dclass == null)
      throw new RuntimeException("no such class: "+dtype.getName());
    boolean wantConstructor = mname.equals("<init>");
    java.lang.reflect.Member[] methods;
    if (wantConstructor)
      methods = getConstructors(dclass, modifiers, modmask);
    else
      methods = getMethods(dclass, mname, modifiers, modmask);
    GenericProc gproc = null;
    PrimProcedure pproc = null;
    for (int i = 0;  i < methods.length;  i++)
      {
        gnu.bytecode.Method method;
        if (wantConstructor)
          {
            java.lang.reflect.Constructor rmethod
              = (java.lang.reflect.Constructor) methods[i];
            Class[] rparams = rmethod.getParameterTypes();
            int j = rparams.length;
            Type[] params = new Type[j];
            while (--j >= 0)
              params[j] = interpreter.getTypeFor(rparams[j]);
            method
              = dtype.addMethod("<init>", rmethod.getModifiers(), params,
                                Type.void_type);
          }
        else
          {
            java.lang.reflect.Method rmethod
              = (java.lang.reflect.Method) methods[i];
            Class[] rparams = rmethod.getParameterTypes();
            int j = rparams.length;
            Type[] params = new Type[j];
            while (--j >= 0)
              params[j] = interpreter.getTypeFor(rparams[j]);
            method
              = dtype.addMethod(rmethod.getName(),
                                rmethod.getModifiers(), params,
                                interpreter.getTypeFor(rmethod.getReturnType()));
          }
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
  public static int selectMethods(java.lang.reflect.Member[] methods,
                                  String mname, int modifiers, int modmask)
  {
    int n = methods.length;
    String mnameV = mname+"$V";
    int num = 0;
    for (int i = 0;  i < n;  i++)
      {
        java.lang.reflect.Member method = methods[i];
        if (mname != null)
          {
            String methodName = method.getName(); 
            if (! mname.equals(methodName)
                && ! mnameV.equals(methodName))
              continue;
          }
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

  public static java.lang.reflect.Constructor[]
  getConstructors(Class clas, int modifiers, int modmask)
  {
    java.lang.reflect.Constructor[] methods = clas.getConstructors();
    int nmethods = selectMethods(methods, null, modifiers, modmask);
    if (nmethods == methods.length)
      return methods;
    java.lang.reflect.Constructor[] matching
    = new java.lang.reflect.Constructor[nmethods];
    System.arraycopy(methods, 0, matching, 0, nmethods);
    return matching;
  }
}
