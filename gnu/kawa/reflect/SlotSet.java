package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.lists.FString;
import gnu.expr.*;

public class SlotSet extends Procedure3 implements Inlineable
{
  /** True if this is a "static-field" operation. */
  boolean isStatic;

  /** Return value is the first argument, rather than void.
   * Only if non-static. */
  boolean returnSelf;

  public static final SlotSet set$Mnfield$Ex = new SlotSet("set-field!", false);
  public static final SlotSet set$Mnstatic$Mnfield$Ex
  = new SlotSet("set-static-field!", true);
  public static final SlotSet setFieldReturnObject
    = new SlotSet("set-field-return-object!", false);
  static { setFieldReturnObject.returnSelf = true; }

  public SlotSet(String name, boolean isStatic)
  {
    super(name);
    this.isStatic = isStatic;
    setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.reflect.CompileReflect:validateApplySlotSet");
  }

  public static void setField (Object obj, String name, Object value)
  {
    apply(false, obj, name, value);
  }

  public static void setStaticField (Object obj, String name, Object value)
  {
    apply(true, obj, name, value);
  }

  public static void apply (boolean isStatic, Object obj, Object member, Object value)
  {
    Language language = Language.getDefaultLanguage();
    boolean illegalAccess = false;
    String name;
    String fname;
    Class clas;
    if (member instanceof String
        || member instanceof FString
        || member instanceof Symbol) {
      name = member.toString();
      fname = gnu.expr.Compilation.mangleNameIfNeeded(name);
      clas = isStatic ? SlotGet.coerceToClass(obj) : obj.getClass();
    }
    else {
      fname = name = ((Member) member).getName();
      clas = null;
    }
    try
      {
        java.lang.reflect.Field field
          = member instanceof Field ? ((Field) member).getReflectField()
          : clas.getField(fname);
	Class ftype = field.getType();
        field.set(obj, language.coerceFromObject(ftype, value));
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
    // First look for "getName" or "isName", to get the "field type".
    try
      {
        java.lang.reflect.Method getmethod = null;
    
        boolean haveSetter = member instanceof Method;
	String setName = haveSetter ? fname
          : ClassExp.slotToMethodName("set", name);
        if (haveSetter && ! setName.startsWith("set"))
          haveSetter = false;

        try {
          String getName = haveSetter ? "get" + setName.substring(3)
            : ClassExp.slotToMethodName("get", name);
          getmethod = clas.getMethod(getName, SlotGet.noClasses);
        } catch (Exception getEx) {
          String getName = haveSetter ? "is" + setName.substring(3)
            : ClassExp.slotToMethodName("is", name);
          getmethod = clas.getMethod(getName, SlotGet.noClasses);
        }
        
        Class[] setArgTypes = new Class[1];
        setArgTypes[0] = getmethod.getReturnType();
        java.lang.reflect.Method setmethod
          = clas.getMethod(setName, setArgTypes);
        Object[] args = new Object[1];
        args[0] = language.coerceFromObject(setArgTypes[0], value);
        setmethod.invoke(obj, args);
        return;
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
      throw new RuntimeException("illegal access for field "+name);
    else
      throw new RuntimeException ("no such field "+name
                                  +" in "+clas.getName());
  }

  public Object apply3 (Object obj, Object fname, Object value)
  {
    apply(isStatic, obj, fname, value);
    return returnSelf ? obj : Values.empty;
  }

  static final Type[] type1Array = new Type[1];

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

    String setName = ClassExp.slotToMethodName("set", name);
    gnu.bytecode.Method method = clas.getMethod(setName, type1Array);
    if (method == null)
      return field;
    else
      return method;
  }

  static void compileSet(Procedure thisProc, ObjectType ctype,
                         Expression valArg, Object part, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    Language language = comp.getLanguage();
    boolean isStatic
      = thisProc instanceof SlotSet && ((SlotSet) thisProc).isStatic;
    if (part instanceof gnu.bytecode.Field)
      {
        gnu.bytecode.Field field = (gnu.bytecode.Field) part;
        boolean isStaticField = field.getStaticFlag();
	Type ftype = language.getLangTypeFor(field.getType());
        if (isStatic && ! isStaticField)
          comp.error('e', ("cannot access non-static field `" + field.getName()
                           + "' using `" + thisProc.getName() + '\''));
        valArg.compile(comp, CheckedTarget.getInstance(ftype));
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
        valArg.compile(comp, CheckedTarget.getInstance(language.getLangTypeFor(setArgTypes[0])));
        if (isStaticMethod)
          code.emitInvokeStatic(method);
        else
          code.emitInvoke(method);
        if (! method.getReturnType().isVoid())
          code.emitPop(1);
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
    Member part = null;
    if (type instanceof ObjectType && arg1 instanceof QuoteExp)
      {
        Object val1 = ((QuoteExp) arg1).getValue();
        ObjectType ctype = (ObjectType) type;
	String name;
        ClassType caller = comp.curClass != null ? comp.curClass
          : comp.mainClass;
        if (val1 instanceof String
            || val1 instanceof FString
            || val1 instanceof Symbol)
	  {
            name = val1.toString();
	    part = lookupMember(ctype, name, caller);
	    if (part == null && type != Type.pointer_type
                && comp.warnUnknownMember())
	      comp.error('w', "no slot `"+name+"' in "+ctype.getName());
	  }
	else if (val1 instanceof Member)
	  {
	    // Inlining (make <type> field: value) creates calls to
	    // setFieldReturnObject whose 2nd arg is a Field or Method.
            part = (Member) val1;
            name = part.getName();
	  }
        else
          name = null;

	if (part != null)
	  {
	    int modifiers = part.getModifiers();
	    boolean isStaticField = (modifiers & Access.STATIC) != 0;
	    if (caller != null && ! caller.isAccessible(part, ctype))
	      comp.error('e', "slot '"+name
                         +"' in "+part.getDeclaringClass().getName()
			 +" not accessible here");
	    args[0].compile(comp,
			    isStaticField ? Target.Ignore
			    : Target.pushValue(ctype));
	    if (returnSelf)
	      comp.getCode().emitDup(ctype.getImplementationType());
	    compileSet(this, ctype, args[2], part, comp);
	    if (returnSelf)
	      target.compileFromStack(comp, ctype);
	    else
	      comp.compileConstant(Values.empty, target);
	    return;
	  }
      }
    ApplyExp.compile(exp, comp, target);
  }
}
