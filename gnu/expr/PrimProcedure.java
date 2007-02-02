// Copyright (c) 1999, 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/** A primitive Procedure implemented by a plain Java method. */

public class PrimProcedure extends MethodProc implements gnu.expr.Inlineable
{
  Type retType;
  /** The types of the method parameters.
   * If known, the types have been coerced to Language-specific parameters.
   * Does not include the implicit static link argument of some constructors.
   */
  Type[] argTypes;
  Method method;
  int op_code;
  /** 'P' means use invokespecial;
   * 'V' means expect a target (this) argument, even if method is static;
   * '\0' means don't expect a target. */
  char mode;

  /** If non-null, the LambdaExp that this PrimProcedure implements. */
  LambdaExp source;

  java.lang.reflect.Member member;

  public final int opcode() { return op_code; }

  public Type getReturnType () { return retType; }
  public void setReturnType (Type retType) { this.retType = retType; }

  public boolean isSpecial() { return mode == 'P'; }

  public Type getReturnType (Expression[] args) { return retType; }

  public Method getMethod () { return method; }

  /** Return true iff the last parameter is a "rest" argument. */
  public boolean takesVarArgs()
  {
    if (method != null)
      {
	String name = method.getName();
	return name.endsWith("$V") || name.endsWith("$V$X");
      }
    return false;
  }

  public boolean takesContext()
  {
    return method != null && takesContext(method);
  }

  public static boolean takesContext(Method method)
  {
    return method.getName().endsWith("$X");
  }

  public final boolean isConstructor()
  {
    // invokespecial == primitive-constructor
    return opcode() == 183 && mode != 'P';
  }

  /** Whether we are passed an argument for the 'target' / 'receiver' / 'this'.
   * Normally this is false for static methods and true for non-static
   * methods.  However, we may need to be able to call a static method using
   * {@code object.name(args...)} (Java syntax) or
   * {@code (invoke object 'name args...)} (Scheme syntax).
   * This includes when the {@code object} is implied.
   * In this case we need to ignore the first argument's value.
   */
  public boolean takesTarget ()
  {
    return mode != '\0';
  }

  /** The (minimum, number) of arguments.
   * Doesn't not count implicit CallContext argument.
   * Does count 'this' argument for non-static methods.
   * Does count an implicit staticLink argument for constructor.
   */
  public int numArgs()
  {
    int num = argTypes.length;
    if (takesTarget())
      num++;
    if (takesContext())
      num--;
    return takesVarArgs() ? (num - 1) + (-1 << 12) : num + (num << 12);
  }

  public int match0 (CallContext ctx)
  {
    return matchN(ProcedureN.noArgs, ctx);
  }

  public int match1 (Object arg1, CallContext ctx)
  {
    Object[] args = { arg1 };
    return matchN(args, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    Object[] args = { arg1, arg2 };
    return matchN(args, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    Object[] args = { arg1, arg2, arg3 };
    return matchN(args, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    Object[] args = { arg1, arg2, arg3, arg4 };
    return matchN(args, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    int nargs = args.length;
    boolean takesVarArgs = takesVarArgs();
    int fixArgs = minArgs();
    if (nargs < fixArgs)
      return NO_MATCH_TOO_FEW_ARGS|fixArgs;
    if (! takesVarArgs && nargs > fixArgs)
      return NO_MATCH_TOO_MANY_ARGS|fixArgs;
    int paramCount = argTypes.length;
    Type elementType = null;
    Object[] restArray = null;
    int extraCount = (takesTarget() || isConstructor()) ? 1 : 0;
    boolean takesContext = takesContext();
    Object[] rargs = new Object[paramCount];
    if (takesContext)
      rargs[--paramCount] = ctx;
    Object extraArg;
    if (takesVarArgs)
      {
	Type restType = argTypes[paramCount-1];
	if (restType == Compilation.scmListType)
	  { // FIXME
	    rargs[paramCount-1] = gnu.lists.LList.makeList(args, fixArgs);
	    nargs = fixArgs;
            elementType = Type.pointer_type;
	  }
	else
	  {
	    ArrayType restArrayType = (ArrayType) restType;
	    elementType = restArrayType.getComponentType();
	    Class elementClass = elementType.getReflectClass();
	    restArray = (Object[])
	      java.lang.reflect.Array.newInstance(elementClass, nargs-fixArgs);
	    rargs[paramCount-1] = restArray;
	  }
      }
    if (isConstructor())
      extraArg = args[0];
    else if (extraCount != 0)
      {
	try
	  {
            extraArg = method.getDeclaringClass().coerceFromObject(args[0]);
	  }
	catch (ClassCastException ex)
          {
            return NO_MATCH_BAD_TYPE|1;
          }
      }
    else
      extraArg = null;
    for (int i = extraCount;  i < args.length; i++)
      {
        Object arg = args[i];
        Type type = i < fixArgs ? argTypes[i-extraCount] : elementType;
        if (type != Type.pointer_type)
          {
            try
              {
                arg = type.coerceFromObject(arg);
              }
            catch (ClassCastException ex)
              {
                return NO_MATCH_BAD_TYPE|(i+1);
              }
          }
        if (i < fixArgs)
          rargs[i-extraCount] = arg;
        else if (restArray != null) // I.e. using array rather than LList.
          restArray[i - fixArgs] = arg;
      }
    ctx.value1 = extraArg;
    ctx.values = rargs;
    ctx.proc = this;
    return 0;
  }

  public void apply (CallContext ctx) throws Throwable
  {
    int arg_count = argTypes.length;
    boolean is_constructor = isConstructor();
    boolean slink = is_constructor && method.getDeclaringClass().hasOuterLink();

    try
      {
	if (member == null)
	  {
	    Class clas = method.getDeclaringClass().getReflectClass();
	    Class[] paramTypes = new Class[arg_count+(slink?1:0)];
	    for (int i = arg_count; --i >= 0; )
	      paramTypes[i+(slink?1:0)] = argTypes[i].getReflectClass();
            if (slink)
              paramTypes[0] = method.getDeclaringClass().getOuterLinkType().getReflectClass();
	    if (is_constructor)
	      member = clas.getConstructor(paramTypes);
	    else if (method != Type.clone_method)
	      member = clas.getMethod(method.getName(), paramTypes);
	  }
	Object result;
	if (is_constructor)
          {
            Object[] args = ctx.values;
            if (slink)
              {
                int nargs = args.length + 1;
                Object[] xargs = new Object[nargs];
                System.arraycopy(args, 0, xargs, 1, nargs-1);
                xargs[0] = ((PairClassType) ctx.value1).staticLink;
                args = xargs;
              }

            result = (((java.lang.reflect.Constructor) member)
                      .newInstance(args));
          }
        else if (method == Type.clone_method)
          {
            // The special Type.clone_method is only used for array types.
            Object arr = ctx.value1;
            Class elClass = arr.getClass().getComponentType();
            int n = java.lang.reflect.Array.getLength(arr);
            result = java.lang.reflect.Array.newInstance(elClass, n);
            System.arraycopy(arr, 0, result, 0, n);
          }
	else
	  result = retType.coerceToObject(((java.lang.reflect.Method) member)
					  .invoke(ctx.value1, ctx.values));
        if (! takesContext())
          ctx.consumer.writeObject(result);
      }
    catch (java.lang.reflect.InvocationTargetException ex)
      {
	throw ex.getTargetException();
      }
  }

  public PrimProcedure (String className, String methodName, int numArgs)
  {
    this(ClassType.make(className).getDeclaredMethod(methodName, numArgs));
  }

  public PrimProcedure(java.lang.reflect.Method method, Language language)
  {
    this(((ClassType) language.getTypeFor(method.getDeclaringClass()))
         .getMethod(method), language);
  }

  public PrimProcedure(Method method)
  {
    init(method);
    this.retType = method.getName().endsWith("$X") ? Type.pointer_type
      : method.getReturnType();
  }

  public PrimProcedure(Method method, Language language)
  {
    this(method, '\0', language);
  }

  public PrimProcedure(Method method, char mode, Language language)
  {
    this.mode = mode;

    init(method);

    // This stuff deals with that a language may have its own mapping
    // from Java types to language types, for coercions and other reasons.
    Type[] pTypes = this.argTypes;
    int nTypes = pTypes.length;
    argTypes = null;
    for (int i = nTypes;  --i >= 0; )
      {
	Type javaType = pTypes[i];
        if (javaType instanceof ClassType
            && ! ((ClassType) javaType).isExisting())
          continue;
	Type langType = language.getLangTypeFor(javaType);
	if (javaType != langType)
	  {
	    if (argTypes == null)
	      {
		argTypes = new Type[nTypes];
		System.arraycopy(pTypes, 0, argTypes, 0, nTypes); 
	      }
	    argTypes[i] = langType;
	  }
      }
    if (argTypes == null)
      argTypes = pTypes;
    if (isConstructor())
      retType = method.getDeclaringClass();
    else if (method.getName().endsWith("$X"))
      retType = Type.pointer_type;
    else
      {
        retType = language.getLangTypeFor(method.getReturnType());

        // Kludge - tostring_type doesn't have methods.
        // It shouldn't be used as the "type" of anything -
        // it's just a type with a coercion.  FIXME.
        if (retType == Type.tostring_type)
          retType = Type.string_type;
      }
  }
  
  private void init(Method method)
  {
    this.method = method;
    int flags = method.getModifiers();
    if ((flags & Access.STATIC) != 0)
      this.op_code = 184;  // invokestatic
    else
      {
	ClassType mclass = method.getDeclaringClass();
	if (mode == 'P')
	  this.op_code = 183;  // invokespecial
        else
          {
            mode = 'V';
            if ("<init>".equals(method.getName()))
              this.op_code = 183;  // invokespecial
            else if ((mclass.getModifiers() & Access.INTERFACE) != 0)
              this.op_code = 185;  // invokeinterface
            else
              this.op_code = 182;  // invokevirtual
          }
      }
    Type[] mtypes = method.getParameterTypes();
    if (isConstructor() && method.getDeclaringClass().hasOuterLink())
      {
        int len = mtypes.length-1;
        Type[] types = new Type[len];
        System.arraycopy(mtypes, 1, types, 0, len);
        mtypes = types;
      }
    this.argTypes = mtypes;
  }

  public PrimProcedure(Method method, LambdaExp source)
  {
    this(method);
    this.retType = source.getReturnType();
    this.source = source;
  }

  public PrimProcedure(int opcode, Type retType, Type[] argTypes)
  {
    this.op_code = opcode;
    this.retType = retType;
    this.argTypes= argTypes;
  }

  public static PrimProcedure makeBuiltinUnary(int opcode, Type type)
  {
    // FIXME - should cache!
    Type[] args = new Type[1];
    args[0] = type;
    return new PrimProcedure(opcode, type, args);
  }

  public static PrimProcedure makeBuiltinBinary(int opcode, Type type)
  {
    // FIXME - should cache!
    Type[] args = new Type[2];
    args[0] = type;
    args[1] = type;
    return new PrimProcedure(opcode, type, args);
  }

  public PrimProcedure(int op_code, ClassType classtype, String name,
		       Type retType, Type[] argTypes)
  {
    this.op_code = op_code;
    method = classtype.addMethod (name, op_code == 184 ? Access.STATIC : 0,
				  argTypes, retType);
    this.retType = retType;
    this.argTypes= argTypes;
    mode = op_code == 184 ? '\0' : 'V';
  }

  /** True if there is no 'this' parameter. */
  public final boolean getStaticFlag()
  {
    return method == null 
      || method.getStaticFlag()
      || isConstructor();
  }

  public final Type[] getParameterTypes() { return argTypes; }

  /** Compile arguments and push unto stack.
   * @param args arguments to evaluate and push.
   * @param thisType If we are calling a non-static function,
   *   then args[0] is the receiver and thisType is its expected class.
   *   If thisType==Type.void_type, ignore argTypes[0].  (It is used to to
   *   pass a link to a closure environment, which was pushed by our caller.)
   *   If thisType==null, no special handling of args[0] or argTypes[0].
   */
  private void compileArgs(Expression[] args, int startArg, Type thisType, Compilation comp)
 {
    boolean variable = takesVarArgs();
    String name = getName();
    Type arg_type = null;
    gnu.bytecode.CodeAttr code = comp.getCode();
    int skipArg = thisType == Type.void_type ? 1 : 0;
    int arg_count = argTypes.length - skipArg;
    if (takesContext())
      arg_count--;
    boolean is_static = thisType == null || skipArg != 0;
    int fix_arg_count = variable ? arg_count - 1 : args.length - startArg;
    Declaration argDecl = source == null ? null : source.firstDecl();
    if (argDecl != null && argDecl.isThisParameter())
      argDecl = argDecl.nextDecl();
    for (int i = 0; ; ++i)
      {
        if (variable && i == fix_arg_count)
          {
            arg_type = argTypes[arg_count-1+skipArg];
	    if (arg_type == Compilation.scmListType)
	      {
		gnu.kawa.functions.MakeList.compile(args, startArg+i, comp);
		break;
	      }
            code.emitPushInt(args.length - startArg - fix_arg_count);
            arg_type = ((ArrayType) arg_type).getComponentType();
            code.emitNewArray(arg_type);
          }
        if (i + startArg >= args.length)
          break;
        if (i >= fix_arg_count)
          {
            code.emitDup(1); // dup array.
            code.emitPushInt(i - fix_arg_count);
          }
        else
          arg_type = argDecl != null && (is_static || i > 0) ? argDecl.getType()
	    : is_static ? argTypes[i + skipArg]
            : i==0 ? thisType
            : argTypes[i-1];
	comp.usedClass(arg_type);
	Target target =
	  source == null ? CheckedTarget.getInstance(arg_type, name, i+1)
	  : CheckedTarget.getInstance(arg_type, source, i);
	args[startArg+i].compileNotePosition(comp, target, args[startArg+i]);
        if (i >= fix_arg_count)
          code.emitArrayStore(arg_type);
	if (argDecl != null && (is_static || i > 0))
	  argDecl = argDecl.nextDecl();
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    ClassType mclass = method == null ? null :  method.getDeclaringClass();
    Expression[] args = exp.getArgs();
    if (isConstructor())
      {
        code.emitNew(mclass);
        code.emitDup(mclass);
      }
    String arg_error = WrongArguments.checkArgCount(this, args.length);
    if (arg_error != null)
      comp.error('e', arg_error);

    compile(getStaticFlag() ? null : mclass, exp, comp, target);
  }

  void compile (Type thisType, ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    gnu.bytecode.CodeAttr code = comp.getCode();
    Type stackType = retType;
    int startArg = 0;
    if (isConstructor())
      {
        ClassType mclass = method == null ? null :  method.getDeclaringClass();
        if (mclass.hasOuterLink())
          {
            // This can be optimized in most cases. FIXME.
            args[0].compile(comp, Target.pushValue(Compilation.typeClassType));
            code.emitInvokeStatic(ClassType.make("gnu.expr.PairClassType").getDeclaredMethod("extractStaticLink", 1));
            code.emitCheckcast(mclass.getOuterLinkType());
          }
        thisType = null;
        startArg = 1;
      }
    else if (takesTarget() && method.getStaticFlag())
      startArg = 1;

    compileArgs(args, startArg, thisType, comp);

    if (method == null)
      {
        code.emitPrimop (opcode(), args.length, retType);
        target.compileFromStack(comp, stackType);
      }
    else
      {
        compileInvoke(comp, method, target,
                      exp.isTailCall(), op_code, stackType);
      }
  }

  /** Emit the actual invoke operation, after arguments have been pushed.
   * Does whatever magic is needed to pass the result to target,
   * including passing CallContext or special handling of ConsumerTarget.
   */
  public static void
  compileInvoke (Compilation comp, Method method, Target target,
                 boolean isTailCall, int op_code, Type stackType)
  {
    CodeAttr code = comp.getCode();
    comp.usedClass(method.getDeclaringClass());
    comp.usedClass(method.getReturnType());
    if (! takesContext(method))
      {
        code.emitInvokeMethod(method, op_code);
      }
    else if (target instanceof IgnoreTarget
               || (target instanceof ConsumerTarget
                 && ((ConsumerTarget) target).isContextTarget()))
      {
        Field consumerFld = null;
        Variable saveCallContext = null;
        comp.loadCallContext();
        if (target instanceof IgnoreTarget)
          {
            ClassType typeCallContext = Compilation.typeCallContext;
            consumerFld = typeCallContext.getDeclaredField("consumer");
            
            // Consumer saveConsumer = ctx.consumer;
            // ctx.consumer = VoidConsumer.instance:
            code.pushScope();
            saveCallContext = code.addLocal(typeCallContext);
            code.emitDup();
            code.emitGetField(consumerFld);
            code.emitStore(saveCallContext);
            code.emitDup();
            code.emitGetStatic(ClassType.make("gnu.lists.VoidConsumer")
                               .getDeclaredField("instance"));
            code.emitPutField(consumerFld);
          }
        code.emitInvokeMethod(method, op_code);
        if (isTailCall)
          {
            comp.loadCallContext();
            code.emitInvoke(Compilation.typeCallContext
                            .getDeclaredMethod("runUntilDone", 0));
          }
        if (target instanceof IgnoreTarget)
          {
            // ctx.consumer = saveConsumer
            comp.loadCallContext();
            code.emitLoad(saveCallContext);
            code.emitPutField(consumerFld);
            code.popScope();
         }
        return;
      }
    else
      {
        comp.loadCallContext();
        stackType = Type.pointer_type;
        code.pushScope();
        Variable saveIndex = code.addLocal(Type.int_type);
        comp.loadCallContext();
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("startFromContext", 0));
        code.emitStore(saveIndex);
        code.emitWithCleanupStart();
        code.emitInvokeMethod(method, op_code);
        code.emitWithCleanupCatch(null);
        comp.loadCallContext();
        code.emitLoad(saveIndex);
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("cleanupFromContext", 1));
        code.emitWithCleanupDone();
        comp.loadCallContext();
        code.emitLoad(saveIndex);
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("getFromContext", 1));
        code.popScope();
      }
    target.compileFromStack(comp, stackType);
  }

  public Type getParameterType(int index)
  {
    if (takesTarget())
      {
        if (index == 0)
          return isConstructor() ? Type.pointer_type
            : method.getDeclaringClass();
        index--;
      }
    int lenTypes = argTypes.length;
    if (index < lenTypes - 1)
      return argTypes[index];
    boolean varArgs = takesVarArgs();
    if (index < lenTypes && ! varArgs)
      return argTypes[index];
    // if (! varArgs) ERROR;
    Type restType = argTypes[lenTypes - 1];
    if (restType instanceof ArrayType)
      return ((ArrayType) restType).getComponentType();
    else // Should be LList or some other Sequence class.
      return Type.pointer_type;
  }

  // This is null in JDK 1.1 and something else in JDK 1.2.
  private static ClassLoader systemClassLoader
  = PrimProcedure.class.getClassLoader();

  public static PrimProcedure getMethodFor (Procedure pproc, Expression[] args)
  {
    return getMethodFor(pproc, null, args, Language.getDefaultLanguage());
  }

  /** Search for a matching static method in a procedure's class.
   * @return a PrimProcedure that is suitable, or null. */
  public static PrimProcedure getMethodFor (Procedure pproc, Declaration decl,
					    Expression[] args,
					    Language language)
  {
    int nargs = args.length;
    Type[] atypes = new Type[nargs];
    for (int i = nargs;  --i >= 0;) atypes[i] = args[i].getType();
    return getMethodFor(pproc, decl, atypes, language);
  }

  public static PrimProcedure getMethodFor (Procedure pproc, Declaration decl,
					    Type[] atypes, Language language)
  {
    if (pproc instanceof GenericProc)
      {
	GenericProc gproc = (GenericProc) pproc;
	MethodProc[] methods = gproc.methods;
	pproc = null;
	for (int i = gproc.count;  --i >= 0; )
	  {
	    int applic = methods[i].isApplicable(atypes);
	    if (applic < 0)
	      continue;
	    if (pproc != null)
	      return null; // Ambiguous.
	    pproc = methods[i];
	  }
	if (pproc == null)
	  return null;
      }
    if (pproc instanceof PrimProcedure)
      {
	PrimProcedure prproc = (PrimProcedure) pproc;
	if (prproc.isApplicable(atypes) >= 0)
	  return prproc;
      }
    Class pclass = getProcedureClass(pproc);
    if (pclass == null)
      return null;
    return getMethodFor((ClassType) Type.make(pclass), pproc.getName(),
			decl, atypes, language);
  }

  public static Class getProcedureClass (Object pproc)
  {
    Class procClass;
    if (pproc instanceof ModuleMethod)
      procClass = ((ModuleMethod) pproc).module.getClass();
    else
      procClass = pproc.getClass();
    try
      {
	if (procClass.getClassLoader() == systemClassLoader)
	  return procClass;
      }
    catch (SecurityException ex)
      {
      }
    return null;
  }

  /** Get PrimProcedure for matching method in given class. */
  public static PrimProcedure
  getMethodFor (Class procClass, String name, Declaration decl,
                Expression[] args, Language language)
  {
    return getMethodFor((ClassType) Type.make(procClass),
			name, decl, args, language);
  }

  public static PrimProcedure
  getMethodFor (ClassType procClass, String name, Declaration decl,
                Expression[] args, Language language)
  {
    int nargs = args.length;
    Type[] atypes = new Type[nargs];
    for (int i = nargs;  --i >= 0;) atypes[i] = args[i].getType();
    return getMethodFor(procClass, name, decl, atypes, language);
  }

  public static PrimProcedure
  getMethodFor (ClassType procClass, String name, Declaration decl,
		Type[] atypes, Language language)
  {
    PrimProcedure best = null;
    int bestCode = -1;
    boolean bestIsApply = false;
    try
      {
        if (name == null)
          return null;
        String mangledName = Compilation.mangleName(name);
        String mangledNameV = mangledName + "$V";
        String mangledNameVX = mangledName + "$V$X";
        String mangledNameX = mangledName + "$X";
	boolean applyOk = true; // Also look for "apply" and "apply$V".
	for (Method meth = procClass.getDeclaredMethods();
	   meth != null;  meth = meth.getNext())
          {
            int mods = meth.getModifiers();
            if ((mods & (Access.STATIC|Access.PUBLIC))
                != (Access.STATIC|Access.PUBLIC))
	      {
		if (decl == null || decl.base == null)
		  continue;
	      }
            String mname = meth.getName();
	    boolean isApply;
	    if (mname.equals(mangledName)
		|| mname.equals(mangledNameV)
		|| mname.equals(mangledNameX)
		|| mname.equals(mangledNameVX))
	      {
		isApply = false;
	      }
	    else if (applyOk
		     && (mname.equals("apply") || mname.equals("apply$V")))
	      {
		isApply = true;
	      }
            else
              continue;
	    if (! isApply)
	      {
		// If we saw a real match, ignore "apply".
		applyOk = false;
		if (bestIsApply)
		  {
		    best = null;
		    bestCode = -1;
		    bestIsApply = false;
		  }
	      }
	    PrimProcedure prproc = new PrimProcedure(meth, language);
	    prproc.setName(name);
	    int code = prproc.isApplicable(atypes);
	    if (code < 0 || code < bestCode)
	      continue;
	    if (code > bestCode)
	      {
		best = prproc;
	      }
	    else if (best != null)
	      {
		best = (PrimProcedure) MethodProc.mostSpecific(best, prproc);
		if (best == null)
		  { // Ambiguous.
		    if (bestCode > 0)
		      return null;
		  }
	      }
	    bestCode = code;
	    bestIsApply = isApply;
          }
      }
    catch (SecurityException ex)
      {
      }
    return best;
  }

  public String getName()
  {
    String name = super.getName();
    if (name != null)
      return name;
    name = getVerboseName();
    setName(name);
    return name;
  }

  public String getVerboseName()
  {
    StringBuffer buf = new StringBuffer(100);
    if (method == null)
      {
	buf.append("<op ");
	buf.append(op_code);
	buf.append('>');
      }
    else
      {
	buf.append(method.getDeclaringClass().getName());
	buf.append('.');
	buf.append(method.getName());
      }
    buf.append('(');
    for (int i = 0; i < argTypes.length; i++)
      {
	if (i > 0)
	  buf.append(',');
	buf.append(argTypes[i].getName());
      }
    buf.append(')');
    return buf.toString();
  }


  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append(retType.getName());
    buf.append(' ');
    buf.append(getVerboseName());
    return buf.toString();
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<primitive procedure ");
    ps.print(toString());
    ps.print ('>');
  }
}
