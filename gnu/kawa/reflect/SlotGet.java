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
  public static final SlotGet slotRef
    = new SlotGet("slot-ref", false, SlotSet.set$Mnfield$Ex);
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

  public Object apply2 (Object arg1, Object arg2)
  {
    String name, fname;
    String getName = null, isName = null;
    if (arg2 instanceof gnu.bytecode.Field)
      {
        fname = ((gnu.bytecode.Field) arg2).getName();
        name = Compilation.demangleName(fname, true);
      }
    else if (arg2 instanceof gnu.bytecode.Method)
      {
        String mname = ((gnu.bytecode.Method) arg2).getName();
        name = Compilation.demangleName(mname, false);
        if (mname.startsWith("get"))
          getName = mname;
        else if (mname.startsWith("is"))
          isName = mname;
        fname = null;
      }
    else if (arg2 instanceof SimpleSymbol
             /* #ifdef use:java.lang.CharSequence */
             || arg2 instanceof CharSequence
             /* #else */
             // || arg2 instanceof gnu.lists.CharSeq || arg2 instanceof String
             /* #endif */
             )
      {
        name = arg2.toString();
        fname = gnu.expr.Compilation.mangleNameIfNeeded(name);
      }
    else
      throw new WrongType(this, 2, arg2, "string");
    // "intern" fname if it is "class" or "length":
    if ("class".equals(fname))
      fname = "class";
    else if ("length".equals(fname))
      fname = "length";
    return getSlotValue(isStatic, arg1, name, fname, getName, isName,
                         Language.getDefaultLanguage());
  }

  /** The actual gets of finding the field value.
   * The compiler emits calls to this method if the field name is literals
   * but the actual field is not known at compile time.
   * This speeds lookup a bit.
   */
  public static Object
  getSlotValue (boolean isStatic, Object obj, String name, String fname,
                String getName, String isName, Language language)
  {
    Class clas = isStatic ? coerceToClass(obj) : obj.getClass();
    if (fname == "length" && clas.isArray())
      {
	int length = java.lang.reflect.Array.getLength(obj);
	return language.coerceToObject(length);
      }
    if (fname == "class")
      return clas;
    boolean illegalAccess = false;
    if (fname != null)
      {
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
              throw new RuntimeException("cannot access non-static field '"
                                         + fname + '\'');
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
                ex.printStackTrace();  // FIXME?
              }
          }
      }

    // Try looking for a method "getFname" or "isFname" instead:
    try
      {
        String mname = null;
        java.lang.reflect.Method getmethod = null;
        
        try {
          mname = getName != null ? getName
            : ClassExp.slotToMethodName("get", name);
          getmethod = clas.getMethod(mname, noClasses);
        } catch (Exception getEx) {
          mname = isName != null ? isName
            : ClassExp.slotToMethodName("is", name);
          getmethod = clas.getMethod(mname, noClasses);
        }

        if (isStatic
            && (getmethod.getModifiers() & java.lang.reflect.Modifier.STATIC) == 0)
          throw new RuntimeException("cannot call non-static getter method '"
                                     + mname + '\'');
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
   * @param clas the class type declaring the property.
   * @param name the source (unmangled) name of the property.
   */
  public static Member
  lookupMember (ObjectType clas, String name, ClassType caller)
  {
    gnu.bytecode.Field field
      = clas.getField(Compilation.mangleNameIfNeeded(name), -1);
    if (field != null)
      {
        if (caller == null)
          caller = Type.pointer_type;
        if (caller.isAccessible(field, clas))
          return field;
      }

    // Try looking for a method "getFname" instead:
    String getname = ClassExp.slotToMethodName("get", name);
    gnu.bytecode.Method method = clas.getMethod(getname, Type.typeArray0);
    if (method == null)
      return field;
    else
      return method;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Compilation comp = walker.getCompilation();
    Language language = comp.getLanguage();
    Type type;
    Expression[] args = exp.getArgs();
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    String name = null;
    if (arg1 instanceof QuoteExp)
      {
        Object val1 = ((QuoteExp) arg1).getValue();
        if (val1 instanceof String
            || val1 instanceof FString
            || val1 instanceof Symbol)
          name = val1.toString();
      }
    if (isStatic)
      {
        type = language.getTypeFor(arg0);
        int known = Invoke.checkKnownClass(type, comp);
        if (known < 0)
          return exp;
        if ("class".equals(name))
          {
            if (known > 0)
              return QuoteExp.getInstance(type.getReflectClass());
            Method method
              = Compilation.typeType.getDeclaredMethod("getReflectClass", 0);
            return new ApplyExp(method, new Expression[] { arg0 });
          }
        if (type != null)
          {
            Expression[] nargs
              = new Expression[] { new QuoteExp(type), arg1 };
            ApplyExp nexp = new ApplyExp(exp.getFunction(), nargs);
            nexp.setLine(exp);
            exp = nexp;
          }
      }
    else
      type = arg0.getType();
    if (type instanceof ClassType && name != null)
      {
	ClassType ctype = (ClassType) type;
	ClassType caller = comp.curClass != null ? comp.curClass
	  : comp.mainClass;
        Member part = lookupMember(ctype, name, caller);
        if (part instanceof gnu.bytecode.Field)
          {
            gnu.bytecode.Field field = (gnu.bytecode.Field) part;
            int modifiers = field.getModifiers();
            boolean isStaticField = (modifiers & Access.STATIC) != 0;
            if (isStatic && ! isStaticField)
              return new ErrorExp("cannot access non-static field `" + name
                                  + "' using `" + getName() + '\'', comp);
	    if (caller != null
                && ! caller.isAccessible(field, ctype))
	      return new ErrorExp("field "+field.getDeclaringClass().getName()
                                  +'.'+name+" is not accessible here", comp);
          }

        else if (part instanceof gnu.bytecode.Method)
          {
            gnu.bytecode.Method method = (gnu.bytecode.Method) part;
            ClassType dtype = method.getDeclaringClass();
	    int modifiers = method.getModifiers();
            boolean isStaticMethod = method.getStaticFlag();
            if (isStatic && ! isStaticMethod)
              return new ErrorExp("cannot call non-static getter method `"
                                  + name + "' using `" + getName() + '\'', comp);
	    if (caller != null && ! caller.isAccessible(dtype, ctype, modifiers))
	      return new ErrorExp( "method "+method +" is not accessible here", 
                                   comp);
          }
        if (part != null)
          {
            Expression[] nargs
              = new Expression[] { arg0, new QuoteExp(part) };
            ApplyExp nexp = new ApplyExp(exp.getFunction(), nargs);
            nexp.setLine(exp);
            return nexp;
          }
        if (type != Type.pointer_type)
          comp.error('e', "no slot `"+name+"' in "+ctype.getName());
      }
    if (name != null && ! (type instanceof ArrayType))
      {
        String fname = gnu.expr.Compilation.mangleNameIfNeeded(name);
        // So we can quickly check for "class" or "length".
        // The name gets interned anyway when compiled.
        fname = fname.intern();
        String getName = ClassExp.slotToMethodName("get", name);
        String isName = ClassExp.slotToMethodName("is", name);
        ApplyExp nexp
          = new ApplyExp(Invoke.invokeStatic,
                         new Expression[] {
                           QuoteExp.getInstance("gnu.kawa.reflect.SlotGet"),
                           QuoteExp.getInstance("getSlotValue"),
                           isStatic ? QuoteExp.trueExp : QuoteExp.falseExp,
                           args[0],
                           QuoteExp.getInstance(name),
                           QuoteExp.getInstance(fname),
                           QuoteExp.getInstance(getName),
                           QuoteExp.getInstance(isName),
                           QuoteExp.getInstance(language)});
        nexp.setLine(exp);
        return walker.walkApplyOnly(nexp);
      }
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    Language language = comp.getLanguage();
    Type type = isStatic ? language.getTypeFor(arg0)
      : arg0.getType();
    CodeAttr code = comp.getCode();
    if (type instanceof ClassType && arg1 instanceof QuoteExp)
      {
	ClassType ctype = (ClassType) type;
        Object part = ((QuoteExp) arg1).getValue();
        if (part instanceof gnu.bytecode.Field)
          {
            gnu.bytecode.Field field = (gnu.bytecode.Field) part;
            int modifiers = field.getModifiers();
            boolean isStaticField = (modifiers & Access.STATIC) != 0;
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
	    Class fclass = ftype.getReflectClass();
	    if (fclass != null)
	      ftype = language.getTypeFor(fclass);
	    target.compileFromStack(comp, ftype);
            return;
          }
        if (part instanceof Method)
          {
            gnu.bytecode.Method method = (gnu.bytecode.Method) part;
	    int modifiers = method.getModifiers();
            boolean isStaticMethod = method.getStaticFlag();
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
      }
    String name = ClassMethods.checkName(arg1);
    if (type instanceof ArrayType && "length".equals(name) && ! isStatic)
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
        if (arg1 instanceof QuoteExp)
          {
            Object part = ((QuoteExp) arg1).getValue();
            if (part instanceof gnu.bytecode.Field)
              return ((gnu.bytecode.Field) part).getType();
            if (part instanceof gnu.bytecode.Method)
              return ((gnu.bytecode.Method) part).getReturnType();
            if (! isStatic && arg0.getType() instanceof ArrayType
                && "length".equals(ClassMethods.checkName(arg1, true)))
              return gnu.kawa.lispexpr.LangPrimType.intType;  // FIXME
          }
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
  public static ApplyExp makeGetField(Expression value, String fieldName)
  {
    Expression[] args = new Expression[2];
    args[0] = value;
    args[1] = new QuoteExp(fieldName);
    return new ApplyExp(gnu.kawa.reflect.SlotGet.field, args);
  }
}
