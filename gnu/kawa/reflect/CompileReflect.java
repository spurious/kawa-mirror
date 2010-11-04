package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.lists.FString;

public class CompileReflect
{
  /** Check if class exists.
   * @return 1 if class actually exists;
   * -1 is class should exist, but doesn't;
   * and 0 otherwise.
   */
  public static int checkKnownClass (Type type, Compilation comp)
  {
    if (type instanceof ClassType && type.isExisting())
      {
        try
          {
            type.getReflectClass();
            return 1;
          }
        catch (Exception ex)
          {
            comp.error('e', "unknown class: " + type.getName());
            return -1;
          }
      }
    return 0;
  }

  /** Resolve class specifier to ClassType at inline time.
   * This is an optimization to avoid having a module-level binding
   * created for the class name. */
  public static ApplyExp inlineClassName (ApplyExp exp, int carg,
                                          InlineCalls walker)
  {
    Compilation comp = walker.getCompilation();
    Language language = comp.getLanguage();
    Expression[] args = exp.getArgs();
    if (args.length > carg)
      {
	Type type = language.getTypeFor(args[carg]);
	if (! (type instanceof Type))
	  return exp;
        checkKnownClass(type, comp);
	Expression[] nargs = new Expression[args.length];
	System.arraycopy(args, 0, nargs, 0, args.length);
	nargs[carg] = new QuoteExp(type);
	ApplyExp nexp = new ApplyExp(exp.getFunction(), nargs);
        nexp.setLine(exp);
        return nexp;
      }
    return exp;
  }

  public static Expression validateApplyInstanceOf
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp = inlineClassName(exp, 1, visitor);
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Expression value = args[0];
        Expression texp = args[1];
        if (texp instanceof QuoteExp)
          {
            Object t = ((QuoteExp) texp).getValue();
            if (t instanceof Type)
              {
                Type type = (Type) t;
                if (type instanceof PrimType)
                  type = ((PrimType) type).boxedType();
                if (value instanceof QuoteExp)
                  return type.isInstance(((QuoteExp) value).getValue())
                    ? QuoteExp.trueExp : QuoteExp.falseExp;
                if (! value.side_effects())
                  {
                    int comp = type.compare(value.getType());
                    if (comp == 1 || comp == 0)
                      return QuoteExp.trueExp;
                    if (comp == -3)
                      return QuoteExp.falseExp;
                  }
              }
          }
      }
    return exp;
  }

  public static Expression validateApplySlotGet
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Compilation comp = visitor.getCompilation();
    Language language = comp.getLanguage();
    SlotGet gproc = (SlotGet) proc;
    boolean isStatic = gproc.isStatic;
    Type type;
    Expression[] args = exp.getArgs();
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    Object val1 = arg1.valueIfConstant();
    String name = null;
    if (val1 instanceof String
        || val1 instanceof FString
        || val1 instanceof Symbol)
      name = val1.toString();
    else
      return exp;
    if (isStatic)
      {
        type = language.getTypeFor(arg0);
        int known = checkKnownClass(type, comp);
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
    if (type instanceof ArrayType)
      return exp;
    if (type instanceof ObjectType)
      {
	ObjectType ctype = (ObjectType) type;
	ClassType caller = comp.curClass != null ? comp.curClass
	  : comp.mainClass;
        Member part = gproc.lookupMember(ctype, name, caller);
        if (part instanceof gnu.bytecode.Field)
          {
            gnu.bytecode.Field field = (gnu.bytecode.Field) part;
            int modifiers = field.getModifiers();
            boolean isStaticField = (modifiers & Access.STATIC) != 0;
            if (isStatic && ! isStaticField)
              return new ErrorExp("cannot access non-static field `" + name
                                  + "' using `" + proc.getName() + '\'', comp);
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
                                  + name + "' using `" + proc.getName() + '\'', comp);
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
        if (type != Type.pointer_type && comp.warnUnknownMember())
          comp.error('e', "no slot `"+name+"' in "+ctype.getName());
      }

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
    return visitor.visitApplyOnly(nexp, null); // FIXME
  }

  public static Expression validateApplySlotSet
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    SlotSet sproc = (SlotSet) proc;
    // Unlike, for SlotGet, we do the field-lookup at compile time
    // rather than inline time.  The main reason is that optimizing
    // (set! CLASS-OR-OBJECT:FIELD-NAME VALUE) is tricky, since (currently)
    // afte we've inlined setter, this method doesn't get called.
    boolean isStatic = sproc.isStatic;
    if (isStatic && visitor.getCompilation().mustCompile)
      exp = inlineClassName (exp, 0, visitor);
    exp.setType(sproc.returnSelf && exp.getArgCount() == 3
                ? exp.getArg(0).getType()
                : Type.voidType);
    return exp;
  }

  public static Expression validateApplyTypeSwitch
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    for (int i = 1;  i < args.length;  i++)
      {
	if (args[i] instanceof LambdaExp)
	  {
	    LambdaExp lexp = (LambdaExp) args[i];
	    lexp.setInlineOnly(true);
	    lexp.returnContinuation = exp;
            lexp.inlineHome = visitor.getCurrentLambda();
	  }
      }
    return exp;
  }
}
