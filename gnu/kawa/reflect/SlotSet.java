package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class SlotSet extends Procedure3 implements CanInline, Inlineable
{
  /** True if this is a "static-field" operation. */
  boolean isStatic;

  public static final SlotSet setField$Ex = new SlotSet("set-field!", false);
  public static final SlotSet setStaticField$Ex
  = new SlotSet("set-static-field!", true);

  public SlotSet(String name, boolean isStatic)
  {
    super(name);
    this.isStatic = isStatic;
  }

  public static void setField (Object obj, String name, Object value)
  {
    apply(false, obj, name, value);
  }

  public static void setStaticField (Object obj, String name, Object value)
  {
    apply(true, obj, name, value);
  }

  public static void apply (boolean isStatic, Object obj, String name, Object value)
  {
    Interpreter interpreter = Interpreter.defaultInterpreter; // FIXME
    boolean illegalAccess = false;
    name = gnu.expr.Compilation.mangleNameIfNeeded(name);
    Class clas = isStatic ? SlotGet.coerceToClass(obj) : obj.getClass();
    try
      {
        java.lang.reflect.Field field = clas.getField(name);
	Class ftype = field.getType();
	if ("gnu.mapping.Binding".equals(ftype.getName())
	    && (field.getModifiers() & java.lang.reflect.Modifier.FINAL) != 0)
	  ((Binding) field.get(obj)).set(value);
	else
	  field.set(obj, interpreter.coerceFromObject(ftype, value));
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
    String getName = ClassExp.slotToMethodName("get", name);
    try
      {
        java.lang.reflect.Method getmethod
          = clas.getMethod(getName, SlotGet.noClasses);
	String setName = ClassExp.slotToMethodName("set", name);
        Class[] setArgTypes = new Class[1];
        setArgTypes[0] = getmethod.getReturnType();
        java.lang.reflect.Method setmethod
          = clas.getMethod(setName, setArgTypes);
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
    apply(isStatic, obj, (String) fname, value);
    return Values.empty;
  }

  static Object getField(Type type, String name)
  {
    if (type instanceof ClassType && name != null)
      {
        ClassType clas = (ClassType) type;
        gnu.bytecode.Field field
	  = clas.getField(Compilation.mangleNameIfNeeded(name));
        if (field != null)
          return field;

        // Try looking for a method "getFname" instead:
        String getName = ClassExp.slotToMethodName("get", name);
        gnu.bytecode.Method method = clas.getMethod(getName, Type.typeArray0);
        if (method == null)
          return null;
        Type ftype = method.getReturnType();
        String setName = ClassExp.slotToMethodName("set", name);
        Type[] args = new Type[1];
        args[0] = ftype;
        method = clas.getMethod(setName, args);
        return method;
      }
    return null;
  }

  static void compileSet(Procedure thisProc, ClassType ctype,
                         Expression valArg, Object part, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    boolean isStatic
      = thisProc instanceof SlotSet && ((SlotSet) thisProc).isStatic;
    if (part instanceof gnu.bytecode.Field)
      {
        gnu.bytecode.Field field = (gnu.bytecode.Field) part;
        boolean isStaticField = field.getStaticFlag();
	Type ftype = field.getType();
	boolean indirect = ("gnu.mapping.Binding".equals(ftype.getName())
			    && (field.getModifiers() & Access.FINAL) != 0);
        if (isStatic && ! isStaticField)
          comp.error('e', ("cannot access non-static field `" + field.getName()
                           + "' using `" + thisProc.getName() + '\''));
	if (indirect)
	  {
	    if (isStaticField)
	      code.emitGetStatic(field); 
	    else
	      code.emitGetField(field);
	  }
        valArg.compile(comp,
		       indirect ? Target.pushObject : Target.pushValue(ftype));
	if (indirect)
	  code.emitInvokeVirtual(Compilation.typeBinding.getDeclaredMethod
				 ("set", 1));
        else if (isStaticField)
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

  public Expression inline (ApplyExp exp)
  {
    if (isStatic)
      return Invoke.inlineClassName (exp, 0, Interpreter.defaultInterpreter);
    else
      return exp;
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
