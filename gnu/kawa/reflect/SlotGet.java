package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.lists.FString;
import gnu.kawa.lispexpr.LangPrimType;

public class SlotGet extends Procedure2 implements HasSetter, Inlineable
{
  static Class[] noClasses = { };

  /** True if this is a "static-field" operation. */
  boolean isStatic;

  Procedure setter;
  public static SlotGet field = new SlotGet("field", false, SlotSet.setField$Ex);
  public static SlotGet staticField
  = new SlotGet("static-field", true, SlotSet.setStaticField$Ex);

  public SlotGet(String name, boolean isStatic)
  {
    super(name);
    this.isStatic = isStatic;
  }

  public SlotGet(String name, boolean isStatic, Procedure setter)
  {
    super(name);
    this.isStatic = isStatic;
    this.setter = setter;
  }

  public static Object field(Object obj, String fname)
  {
    return field.apply2(obj, fname);
  }

  public static Object staticField(Object obj, String fname)
  {
    return staticField.apply2(obj, fname);
  }

  public Object apply2 (Object obj, Object name)
  {
    if (! (name instanceof String) && ! (name instanceof FString))
      throw WrongType.make(null, this, 1);
    Interpreter interpreter = Interpreter.defaultInterpreter; // FIXME
    String fname = gnu.expr.Compilation.mangleNameIfNeeded(name.toString());
    Class clas = isStatic ? coerceToClass(obj) : obj.getClass();
    if (clas.isArray() && "length".equals(fname))
      {
	int length = java.lang.reflect.Array.getLength(obj);
	return interpreter.coerceToObject(length);
      }
    boolean illegalAccess = false;
    java.lang.reflect.Field field;
    try
      {
        field = clas.getField(fname);
      }
    catch (Exception ex)
      {
        field = null;
      }
    if (field != null)
      {
        if (isStatic
            && (field.getModifiers() & java.lang.reflect.Modifier.STATIC) == 0)
          throw new RuntimeException("cannot access non-static field `"
                                     + fname + "' using `" + getName() + '\'');
        try
          {
            Object result = field.get(obj);
	    if (result instanceof Binding
		&& ((field.getModifiers() & java.lang.reflect.Modifier.FINAL)
		    != 0))
	      result = ((Binding) result).get();
	    else
	      result = interpreter.coerceToObject(field.getType(), result);
            return result;
          }
        catch (IllegalAccessException ex)
          {
            illegalAccess = true;
          }
        catch (Exception ex)
          {
          }
      }

    // Try looking for a method "getFname" instead:
    String mname = ClassExp.slotToMethodName("get", fname);
    try
      {
        java.lang.reflect.Method getmethod
          = clas.getMethod(mname, noClasses);
        if (isStatic
            && (getmethod.getModifiers() & java.lang.reflect.Modifier.STATIC) == 0)
          throw new RuntimeException("cannot call non-static getter method `"
                                     + mname + "' using `" + getName() + '\'');
        Object result = getmethod.invoke(obj, Values.noArgs);
        result = interpreter.coerceToObject(getmethod.getReturnType(), result);
        return result;
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
      throw new RuntimeException("illegal access for field "+fname);
    else
      throw new RuntimeException ("no such field "+fname
                                  +" in "+clas.getName());
  }

  static Class coerceToClass(Object obj)
  {
    if (obj instanceof Class)
      return (Class) obj;
    if (obj instanceof gnu.bytecode.Type)
      return ((gnu.bytecode.Type) obj).getReflectClass();
    throw new RuntimeException("argument is neither Class nor Type");
  }

  public void setN (Object[] args)
  {
    int nargs = args.length;
    if (nargs != 3)
      throw new WrongArguments(getSetter(), nargs);
    set2(args[0], args[1], args[2]);
  }

  public void set2 (Object obj, Object name, Object value)
  {
    SlotSet.apply(isStatic, obj, (String) name, value);
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
        String getname = ClassExp.slotToMethodName("get", name);
        gnu.bytecode.Method method = clas.getMethod(getname, Type.typeArray0);
        return method;
      }
    return null;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (nargs != 2)
      {
        String msg = nargs < 2 ? "too few" : "too many";
        comp.error('e', msg + " arguments to `"+getName()+'\'');
        comp.compileConstant(null, target);
        return;
      }
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    Type type = isStatic ? kawa.standard.Scheme.exp2Type(arg0)
      : arg0.getType();
    String name = ClassMethods.checkName(arg1);
    CodeAttr code = comp.getCode();
    if (type instanceof ClassType && name != null)
      {
	ClassType ctype = (ClassType) type;
        Object part = getField(ctype, name);
        if (part instanceof gnu.bytecode.Field)
          {
            gnu.bytecode.Field field = (gnu.bytecode.Field) part;
            int modifiers = field.getModifiers();
            boolean isStaticField = (modifiers & Access.STATIC) != 0;
            if (isStatic && ! isStaticField)
              comp.error('e', ("cannot access non-static field `" + name
                               + "' using `" + getName() + '\''));
            args[0].compile(comp,
                            isStaticField ? Target.Ignore
                            : Target.pushValue(ctype));
            if (isStaticField)
              {
                boolean inlined = false;
                /*
                FIXME This isn't quite safe.  We should only "inline"
                the value if the field whose initializer is a constant
                expression (JLS 2nd ed 15.28).  We cannot determine this
                using reflection instead we have to parse the .class file.

                Type ftype = field.getType();
                if ((modifiers & Access.FINAL) != 0
                    && ftype instanceof PrimType)
                  {
                    // We inline int final fields.
                    // Other kinds of final fields are less obviously a win.
                    char sig = ftype.getSignature().charAt(0);
                    if (sig != 'F' && sig != 'D' && sig != 'J')
                      {
                        try
                          {
                            java.lang.reflect.Field rfield
                              = field.getReflectField();
                            int val = rfield.getInt(null);
                            code.emitPushInt(val);
                            inlined = true;
                          }
                        catch (Exception ex)
                          {
                          }
                      }
                  }
                */
                if (! inlined)
                  code.emitGetStatic(field); 
              }
            else
              code.emitGetField(field);
	    Type ftype = field.getType();
	    if ("gnu.mapping.Binding".equals(ftype.getName())
		&& (modifiers & Access.FINAL) != 0)
	      code.emitInvokeVirtual(Compilation.getLocationMethod);
	    target.compileFromStack(comp, ftype);
            return;
          }
        if (part instanceof gnu.bytecode.Method)
          {
            gnu.bytecode.Method method = (gnu.bytecode.Method) part;
            boolean isStaticMethod = method.getStaticFlag();
            if (isStatic && ! isStaticMethod)
              comp.error('e', "cannot call non-static getter method `"
                         + name + "' using `" + getName() + '\'');
            args[0].compile(comp,
                            isStaticMethod ? Target.Ignore
                            : Target.pushValue(ctype));
            if (isStaticMethod)
              code.emitInvokeStatic(method);
            else if (ctype.isInterface())
              code.emitInvokeInterface(method);
            else
              code.emitInvokeVirtual(method);
	    target.compileFromStack(comp, method.getReturnType());
            return;
          }
        if (type != Type.pointer_type)
          comp.error('e', "no slot `"+name+"' in "+ctype.getName());
      }
    else if (type instanceof ArrayType && "length".equals(name) && ! isStatic)
      {
	args[0].compile(comp, Target.pushValue(type));
	code.emitArrayLength();
	target.compileFromStack(comp, LangPrimType.intType);  // FIXME
	return;
      }
    ApplyExp.compile(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    int nargs = args.length;
    if (nargs == 2)
      {
        Expression arg0 = args[0];
        Expression arg1 = args[1];
        Type type = isStatic ? kawa.standard.Scheme.exp2Type(arg0)
          : arg0.getType();
        String name = ClassMethods.checkName(arg1);
        if (type instanceof ClassType && name != null)
          {
            ClassType ctype = (ClassType) type;
            Object part = getField(ctype, name);
            if (part instanceof gnu.bytecode.Field)
              return ((gnu.bytecode.Field) part).getType();
            if (part instanceof gnu.bytecode.Method)
              return ((gnu.bytecode.Method) part).getReturnType();
          }
	else if (type instanceof ArrayType && "length".equals(name)
		 && ! isStatic)
	  return gnu.kawa.lispexpr.LangPrimType.intType;  // FIXME
      }
    return Type.pointer_type;
  }

  public Procedure getSetter()
  {
    return setter == null ? super.getSetter() : setter;
  }

  /**
   * Convenience method to make an Expression that gets the value of a field.
   * @param value evaluates to object that has the named field
   * @param fieldName name of field in value
   * @return expression that get the name field from value
   */
  public static Expression makeGetField(Expression value, String fieldName)
  {
    Expression[] args = new Expression[2];
    args[0] = value;
    args[1] = new QuoteExp(fieldName);
    return new ApplyExp(gnu.kawa.reflect.SlotGet.field, args);
  }
}
