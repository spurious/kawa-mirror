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
    MethodProc result = apply(dtype, mname, rtype, atypes, modifiers, modmask);
    if (result == null)
      throw new RuntimeException("no applicable method named `"+mname+"' in "
                                 +dtype.getName());
    return result;
  }

  /** Return the methods of a class wit the specified name and flag.
   * @return an array containing the methods.
   */
  public static PrimProcedure[] getMethods(ClassType dtype, String mname,
                                           int modifiers, int modmask,
                                           kawa.lang.Interpreter interpreter)
  {
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
    PrimProcedure[] result = new PrimProcedure[methods.length];
    for (int i = 0;  i < methods.length;  i++)
      {
        gnu.bytecode.Method method;
        PrimProcedure cur;
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
            cur = new PrimProcedure(method);
            cur.setReturnType(dtype);
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
            cur = new PrimProcedure(method);
          }
        result[i] = cur;
      }
    return result;
  }

  /** Re-order the methods such that the ones that are definite
   * applicable (all argtypes is subset of parameter type) are first;
   * those possibly applicable next (argtype overlaps parameter types);
   * and ending with those definitely not applicable (some argtype does
   * overlap its parameter type).
   * @return ((number of definitely applicabable methods) << 32
   *          + (number of possibly applicable methods.
   */
  public static long selectApplicable(PrimProcedure[] methods,
                                      Type[] atypes)
  {
    int limit = methods.length;
    int numDefApplicable = 0;
    int numPosApplicable = 0;
    for (int i = 0;  i < limit;  )
      {
        int code = methods[i].isApplicable(atypes);
        if (code < 0)
          { // Definitely not applicable.
            // swap(methods[limit-1], methods[i]):
            PrimProcedure tmp = methods[limit-1];
            methods[limit-1] = methods[i];
            methods[i] = tmp;
            limit--;
          }
        else if (code > 0)
          { // Definitely applicable.
            // swap(methods[numDefApplicable], methods[i]):
            PrimProcedure tmp = methods[numDefApplicable];
            methods[numDefApplicable] = methods[i];
            methods[i] = tmp;
            numDefApplicable++;
            i++;
          }
        else
          { // Possibly applicable.
            numPosApplicable++;
            i++;
          }
      }
    return (((long) numDefApplicable) << 32) + (long) numPosApplicable;
  }

  public static MethodProc apply(ClassType dtype, String mname,
                                 Type rtype, Type[] atypes,
                                 int modifiers, int modmask)
  {
    kawa.lang.Interpreter interpreter = kawa.standard.Scheme.getInstance();
    PrimProcedure[] methods = getMethods(dtype, mname,
                                         modifiers, modmask, interpreter);
    GenericProc gproc = null;
    PrimProcedure pproc = null;
    for (int i = 0;  i < methods.length;  i++)
      {
        PrimProcedure cur = methods[i];;
        if (atypes != null)
          {
            int applicable = cur.isApplicable(atypes);
            if (applicable == -1)
              continue;
            if (pproc != null)
              {
                MethodProc best = MethodProc.mostSpecific(pproc, cur);
                if (best != null)
                  {
                    if (cur == best)
                      pproc = cur;
                    continue;
                  }
              }
          }
        if (pproc != null && gproc == null)
          {
            gproc = new GenericProc();
            gproc.add(pproc);
          }
        pproc = cur;
        if (gproc != null)
          gproc.add(pproc);
      }
    if (gproc != null)
      {
        gproc.setName(dtype.getName()+"."+mname);
        return gproc;
      }
    return pproc;
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

  static String checkName(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        Object name = ((QuoteExp) exp).getValue();
        if (name instanceof FString || name instanceof String)
          return Compilation.mangleName(name.toString());
      }
    return null;
  }
}
