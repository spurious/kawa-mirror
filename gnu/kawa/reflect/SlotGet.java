package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.lists.FString;
import gnu.kawa.lispexpr.LangPrimType;

public class SlotGet extends Procedure2
  implements HasSetter, CanInline, Inlineable
{
  static Class[] noClasses = { };

  /** True if this is a "static-field" operation. */
  boolean isStatic;

  Procedure setter;
  public static final SlotGet field
    = new SlotGet("field", false, SlotSet.set$Mnfield$Ex);
  public static final SlotGet staticField
    = new SlotGet("static-field", true, SlotSet.set$Mnstatic$Mnfield$Ex);

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
      throw WrongType.make(null, this, 2, name);
    Language language = Language.getDefaultLanguage();
    String fname = gnu.expr.Compilation.mangleNameIfNeeded(name.toString());
    Class clas = isStatic ? coerceToClass(obj) : obj.getClass();
    if (clas.isArray() && "length".equals(fname))
      {
	int length = java.lang.reflect.Array.getLength(obj);
	return language.coerceToObject(length);
      }
    if ("class".equals(fname))
      return clas;
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
            return language.coerceToObject(field.getType(), field.get(obj));
          }
        catch (IllegalAccessException ex)
          {
            illegalAccess = true;
          }
        catch (Exception ex)
          {
            ex.printStackTrace();
          }
      }

    // Try looking for a method "getFname" or "isFname" instead:
    try
      {
        String mname = null;
        java.lang.reflect.Method getmethod = null;
        
        try {
          mname = ClassExp.slotToMethodName("get", fname);
          getmethod = clas.getMethod(mname, noClasses);
        } catch (Exception getEx) {
          mname = ClassExp.slotToMethodName("is", fname);
          getmethod = clas.getMethod(mname, noClasses);
        }

        if (isStatic
            && (getmethod.getModifiers() & java.lang.reflect.Modifier.STATIC) == 0)
          throw new RuntimeException("cannot call non-static getter method `"
                                     + mname + "' using `" + getName() + '\'');
        Object result = getmethod.invoke(obj, Values.noArgs);
        result = language.coerceToObject(getmethod.getReturnType(), result);
        return result;
      }
    catch (java.lang.reflect.InvocationTargetException ex)
      {
        throw WrappedException.wrapIfNeeded(ex.getTargetException());
      }
    catch (IllegalAccessException ex)
      {
        illegalAccess = true;
      }
    catch (java.lang.NoSuchMethodException ex)
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

  /** Get a named property - field or 'get' accessor method.
   * @param type the class type declaring the property.
   * @param name the source (unmangled) name of the property.
   */
  public static Object getField(ClassType clas, String name, ClassType caller)
  {
    gnu.bytecode.Field field
      = clas.getField(Compilation.mangleNameIfNeeded(name), -1);
    if (field != null
        && caller != null
        && caller.isAccessible(field.getDeclaringClass(), field.getModifiers()))
      return field;

    // Try looking for a method "getFname" instead:
    String getname = ClassExp.slotToMethodName("get", name);
    gnu.bytecode.Method method = clas.getMethod(getname, Type.typeArray0);
    if (method == null)
      return field;
    else
      return method;
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    if (isStatic)
      return Invoke.inlineClassName (exp, 0, (InlineCalls) walker);
    else
      return exp;
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
    if (isStatic && "class".equals(name)
        // Ideally we should also handle array types ...  FIXME
        && type instanceof ClassType)
      {
        comp.loadClassRef((ClassType) type);
        target.compileFromStack(comp, Type.java_lang_Class_type);
        return;
      }
    CodeAttr code = comp.getCode();
    if (type instanceof ClassType && name != null)
      {
	ClassType ctype = (ClassType) type;
	ClassType caller = comp.curClass != null ? comp.curClass
	  : comp.mainClass;
        Object part = getField(ctype, name, caller);
        if (part instanceof gnu.bytecode.Field)
          {
            gnu.bytecode.Field field = (gnu.bytecode.Field) part;
            ctype = field.getDeclaringClass();
            int modifiers = field.getModifiers();
            boolean isStaticField = (modifiers & Access.STATIC) != 0;
            if (isStatic && ! isStaticField)
              comp.error('e', ("cannot access non-static field `" + name
                               + "' using `" + getName() + '\''));
	    if (caller != null && ! caller.isAccessible(ctype, modifiers))
	      comp.error('e', "field "+ctype.getName()+'.'+name
			 +" is not accessible here");
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
	    Language language = Language.getDefaultLanguage();
	    Class fclass = ftype.getReflectClass();
	    if (fclass != null)
	      ftype = language.getTypeFor(fclass);
	    target.compileFromStack(comp, ftype);
            return;
          }
        if (part instanceof gnu.bytecode.Method)
          {
            gnu.bytecode.Method method = (gnu.bytecode.Method) part;
            ctype = method.getDeclaringClass();
	    int modifiers = method.getModifiers();
            boolean isStaticMethod = method.getStaticFlag();
            if (isStatic && ! isStaticMethod)
              comp.error('e', "cannot call non-static getter method `"
                         + name + "' using `" + getName() + '\'');
	    if (caller != null && ! caller.isAccessible(ctype, modifiers))
	      comp.error('e', "method "+method +" is not accessible here");
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
        String name = ClassMethods.checkName(arg1, true);
        if (type instanceof ClassType && name != null)
          {
            ClassType ctype = (ClassType) type;
            Object part = getField(ctype, name, null);
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
