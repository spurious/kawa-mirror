package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class SlotSet extends Procedure3 implements Inlineable
{
  /** True if this is a "static-field" operation. */
  static boolean isStatic;

  public static void apply (Object obj, String name, Object value)
  {
    kawa.lang.Interpreter interpreter = kawa.standard.Scheme.getInstance();
    boolean illegalAccess = false;
    name = gnu.expr.Compilation.mangleName(name);
    Class clas = isStatic ? SlotGet.coerceToClass(obj) : obj.getClass();
    try
      {
        java.lang.reflect.Field field = clas.getField(name);
        field.set(obj, interpreter.coerceFromObject(field.getType(), value));
        return;
      }
    catch (java.lang.NoSuchFieldException ex)
      {
      }
    catch (IllegalAccessException ex)
      {
	illegalAccess = true;
      }

    // Try looking for a method "setFname" instead.
    // First look for "getName", to get the "field type".
    StringBuffer namebuf = new StringBuffer(name.length()+3);
    namebuf.append("get");
    namebuf.append(Character.toTitleCase(name.charAt(0)));
    namebuf.append(name.substring(1));
    try
      {
        java.lang.reflect.Method getmethod
          = clas.getMethod(namebuf.toString(), SlotGet.noClasses);
        namebuf.setCharAt(0, 's');
        Class[] setArgTypes = new Class[1];
        setArgTypes[0] = getmethod.getReturnType();
        java.lang.reflect.Method setmethod
          = clas.getMethod(namebuf.toString(), setArgTypes);
        Object[] args = new Object[1];
        args[0] = interpreter.coerceFromObject(setArgTypes[0], value);
        setmethod.invoke(obj, args);
        return;
      }
    catch (java.lang.reflect.InvocationTargetException ex2)
      {
        Throwable th = ex2.getTargetException();
        if (th instanceof RuntimeException)
          throw (RuntimeException) th;
        if (th instanceof Error)
          throw (Error) th;
        throw new RuntimeException(th.toString());
      }
    catch (IllegalAccessException ex)
      {
        illegalAccess = true;
      }
    catch (java.lang.NoSuchMethodException ex3)
      {
      }

    if (illegalAccess)
      throw new RuntimeException("illegal access for field "+name);
    else
      throw new RuntimeException ("no such field "+name
                                  +" in "+clas.getName());
  }

  public Object apply3 (Object obj, Object fname, Object value)
  {
    apply(obj, (String) fname, value);
    return Values.empty;
  }

  static Object getField(Type type, String name)
  {
    if (type instanceof ClassType && name != null)
      {
        ClassType clas = (ClassType) type;
        gnu.bytecode.Field field = clas.getField(name);
        if (field != null)
          return field;

        // Try looking for a method "getFname" instead:
        StringBuffer getname = new StringBuffer(name.length()+3);
        getname.append("get");
        getname.append(Character.toTitleCase(name.charAt(0)));
        getname.append(name.substring(1));
        gnu.bytecode.Method method = clas.getMethod(getname.toString(),
                                                    Type.typeArray0);
        if (method == null)
          return null;
        Type ftype = method.getReturnType();
        getname.setCharAt(0, 's');
        Type[] args = new Type[1];
        args[0] = ftype;
        method = clas.getMethod(getname.toString(), args);
        return method;
      }
    return null;
  }

  static void compileSet(Procedure thisProc, ClassType ctype,
                         Expression valArg, Object part, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (part instanceof gnu.bytecode.Field)
      {
        gnu.bytecode.Field field = (gnu.bytecode.Field) part;
        boolean isStaticField = field.getStaticFlag();
        if (isStatic && ! isStaticField)
          comp.error('e', ("cannot access non-static field `" + field.getName()
                           + "' using `" + thisProc.getName() + '\''));
        valArg.compile(comp, Target.pushValue(field.getType()));
        if (isStaticField)
          code.emitPutStatic(field); 
        else
          code.emitPutField(field);
        return;
      }
    if (part instanceof gnu.bytecode.Method)
      {
        gnu.bytecode.Method method = (gnu.bytecode.Method) part;
        boolean isStaticMethod = method.getStaticFlag();
        if (isStatic && ! isStaticMethod)
          comp.error('e', "cannot call non-static getter method `"
                     + method.getName() + "' using `"
                     + thisProc.getName() + '\'');
        Type[] setArgTypes = method.getParameterTypes();
        valArg.compile(comp, Target.pushValue(setArgTypes[0]));
        if (isStaticMethod)
          code.emitInvokeStatic(method);
        else if (ctype.isInterface())
          code.emitInvokeInterface(method);
        else
          code.emitInvokeVirtual(method);
        return;
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (nargs != 3)
      {
        String msg = nargs < 3 ? "too few" : "too many";
        comp.error('e', msg + " arguments to `"+getName()+'\'');
        comp.compileConstant(null, target);
        return;
      }
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    Expression value = args[2];
    Type type = isStatic ? kawa.standard.Scheme.exp2Type(arg0)
      : arg0.getType();
    String name = ClassMethods.checkName(arg1);
    if (type instanceof ClassType && name != null)
      {
        ClassType ctype = (ClassType) type;
        Object part = getField(ctype, name);
        if (part != null)
          {
            boolean isStaticField = 
              (part instanceof gnu.bytecode.Field)
              ? ((gnu.bytecode.Field) part).getStaticFlag()
              : ((gnu.bytecode.Method) part).getStaticFlag();
            args[0].compile(comp,
                            isStaticField ? Target.Ignore
                            : Target.pushValue(ctype));
            compileSet(this, ctype, args[2], part, comp);
            comp.compileConstant(Values.empty, target);
            return;
          }
        if (type != Type.pointer_type)
          comp.error('e', "no slot `"+name+"' in "+ctype.getName());
      }
    ApplyExp.compile(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.void_type;
  }
}
