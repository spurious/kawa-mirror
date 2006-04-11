// Copyright (c) 1999, 2000, 2001, 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.lists.LList;
import java.util.Vector;

/**
 * Class used to implement Scheme lambda expressions.
 * @author	Per Bothner
 */

public class LambdaExp extends ScopeExp
{
  public Expression body;
  /** Minumnum number of parameters.
   * Does not count implicit isThisParameter(). */
  public int min_args;
  /** Maximum number of actual arguments;  -1 if variable. */
  public int max_args;

  /** Set of visible top-level LambdaExps that need apply methods. */
  Vector applyMethods;

  //  public int plainArgs;
  Variable argsArray;
  // First argument that goes into argsArray.
  private Declaration firstArgsArrayArg;

  public Keyword[] keywords;
  public Expression[] defaultArgs;

  /** A list of Declarations, chained using Declaration's nextCapturedVar.
    * All the Declarations are allocated in the current heapFrame. */
  Declaration capturedVars;

  public void capture (Declaration decl)
  {
    if (decl.isSimple())
      {
        if (capturedVars == null
            && ! decl.isStatic()
            && ! (this instanceof ModuleExp || this instanceof ClassExp))
          {
            heapFrame = new gnu.bytecode.Variable("heapFrame");
          }
        decl.setSimple(false);
        if (! decl.isPublic())
          {
            decl.nextCapturedVar = capturedVars;
            capturedVars = decl;
          }
      }
  }

  /** A local variable that points to the heap-allocated part of the frame.
   * Each captured variable is a field in the heapFrame.  A procedure has
   * a heapFrame iff if has a parameter or local variable that is
   * referenced ("captured") by a non-inline inferior procedure.
   * (I.e there is a least one non-inline procedure that encloses the
   * reference but not the definition.)  Note that an inline procedure may
   * have a heapFrame if it encloses a non-inline procedure.  This is
   * necessary because we represent loops as tail-recursive inline procedures.
   */
  Variable heapFrame;

  public LambdaExp firstChild;
  public LambdaExp nextSibling;

  /** A magic value to indicate there is no unique return continuation. */
  final static ApplyExp unknownContinuation = new ApplyExp ((Expression) null, null);

  /** The unique caller that calls this lambda.
      The value is null, if no callers have been seen.
      A value of unknownContinuation means there are multiple call sites.
      Tail-recursive calls do not count as multiple call sites. (With a
      little more analysis, we could also allow multiple non-self tail-calls
      as long as they all are ultimately called from the same place.)
      This is used to see if we can inline the function at its unique
      call site. */
  public ApplyExp returnContinuation;

  /** Expressions that name classes that may be thrown. */
  ReferenceExp[] throwsSpecification;

  public void setExceptions(ReferenceExp[] exceptions)
  {
    throwsSpecification = exceptions;
  }

  /** If non-null, a Declaration whose value is (only) this LambdaExp. */
  public Declaration nameDecl;

  /** If non-null, this is a Field that is used for implementing lexical closures.
   * If getName() is "closureEnv", it is our parent's heapFrame,
   * which is an instance of one of our siblings.
   * (Otherwise, we use "this" as the implicit "closureEnv" field.) */
  public Field closureEnvField;

  /** Field in heapFrame.getType() that contains the static link.
   * It is used by child functions to get to outer environments.
   * Its value is this function's closureEnv value. */
  public Field staticLinkField;

  /** A variable that points to the closure environment passed in.
   * It can be any one of:
   * null, if no closure environment is needed;
   * this, if this object is its parent's heapFrame;
   * a local variable initialized from this.closureEnv;
   * a parameter (only if !getCanRead()); or
   * a copy of our caller's closureEnv or heapFrame (only if getInlineOnly()).
   * See declareClosureEnv and closureEnvField. */
  Variable closureEnv;

  static final int INLINE_ONLY = 1;
  static final int CAN_READ = 2;
  static final int CAN_CALL = 4;
  static final int IMPORTS_LEX_VARS = 8;
  static final int NEEDS_STATIC_LINK = 16;
  /* Used (future) by FindTailCalls. */
  static final int CANNOT_INLINE = 32;
  static final int CLASS_METHOD = 64;
  static final int METHODS_COMPILED = 128;
  public static final int NO_FIELD = 256;
  /** True if any parameter default expression captures a parameter. */
  static final int DEFAULT_CAPTURES_ARG = 512;
  public static final int SEQUENCE_RESULT = 1024;
  protected static final int NEXT_AVAIL_FLAG = 2048;

  /** True iff this lambda is only "called" inline. */
  public final boolean getInlineOnly() { return (flags & INLINE_ONLY) != 0; }
  public final void setInlineOnly(boolean inlineOnly)
  { setFlag(inlineOnly, INLINE_ONLY); }

  public final boolean getNeedsClosureEnv ()
  { return (flags & (NEEDS_STATIC_LINK|IMPORTS_LEX_VARS)) != 0; }

  /** True if a child lambda uses lexical variables from outside.
      Hence, a child heapFrame needs a staticLink to outer frames. */
  public final boolean getNeedsStaticLink ()
  { return (flags & NEEDS_STATIC_LINK) != 0; }

  public final void setNeedsStaticLink(boolean needsStaticLink)
  {
    if (needsStaticLink) flags |= NEEDS_STATIC_LINK;
    else flags &= ~NEEDS_STATIC_LINK;
  }

  /** True iff this lambda "captures" (uses) lexical variables from outside. */
  public final boolean getImportsLexVars ()
  { return (flags & IMPORTS_LEX_VARS) != 0; }

  public final void setImportsLexVars(boolean importsLexVars)
  {
    if (importsLexVars) flags |= IMPORTS_LEX_VARS;
    else flags &= ~IMPORTS_LEX_VARS;
  }

  public final void setImportsLexVars()
  {
    int old = flags;
    flags |= IMPORTS_LEX_VARS;

    // If this needs an environment (closure), then its callers do too.
    if ((old & IMPORTS_LEX_VARS) == 0 && nameDecl != null)
      setCallersNeedStaticLink();
  }

  public final void setNeedsStaticLink()
  {
    int old = flags;
    flags |= NEEDS_STATIC_LINK;

    // If this needs an environment (closure), then its callers do too.
    if ((old & NEEDS_STATIC_LINK) == 0 && nameDecl != null)
      setCallersNeedStaticLink();
  }

  void setCallersNeedStaticLink()
  {
    LambdaExp outer = outerLambda();
    for (ApplyExp app = nameDecl.firstCall;  app != null;  app = app.nextCall)
      {
        LambdaExp caller = app.context;
        for (; caller != outer; caller = caller.outerLambda())
          caller.setNeedsStaticLink();
      }
  }

  public final boolean getCanRead()
  { return (flags & CAN_READ) != 0; }
  public final void setCanRead(boolean read)
  {
    if (read) flags |= CAN_READ;
    else flags &= ~CAN_READ;
  }

  public final boolean getCanCall()
  { return (flags & CAN_CALL) != 0; }
  public final void setCanCall(boolean called)
  {
    if (called) flags |= CAN_CALL;
    else flags &= ~CAN_CALL;
  }

  /** True if this is a method in an ClassExp. */
  public final boolean isClassMethod()
  { return (flags & CLASS_METHOD) != 0; }

  public final void setClassMethod(boolean isMethod)
  {
    if (isMethod) flags |= CLASS_METHOD;
    else flags &= ~CLASS_METHOD;
  }

  /** True iff this is the dummy top-level function of a module body. */
  public final boolean isModuleBody () { return this instanceof ModuleExp; }

  /** True if a class is generated for this procedure.  */
  public final boolean isClassGenerated ()
  {
    return isModuleBody() || this instanceof ClassExp;
  }

  /** Specify the calling convention used for this function.
   * @return One of the CALL_WITH_xxx values in Compilation. */
  public int getCallConvention ()
  {
    if (isModuleBody())
      return ((Compilation.defaultCallConvention
	      >= Compilation.CALL_WITH_CONSUMER)
	      ? Compilation.defaultCallConvention
	      : Compilation.CALL_WITH_CONSUMER);
    if (isClassMethod())
      return Compilation.CALL_WITH_RETURN;
    return ((Compilation.defaultCallConvention
	     != Compilation.CALL_WITH_UNSPECIFIED)
	    ? Compilation.defaultCallConvention
	    : Compilation.CALL_WITH_RETURN);
  }

  public final boolean isHandlingTailCalls ()
  {
    return isModuleBody()
      || (Compilation.defaultCallConvention >= Compilation.CALL_WITH_TAILCALLS
	  && ! isClassMethod());
  }

  public final boolean variable_args () { return max_args < 0; }

  ClassType type = Compilation.typeProcedure;

  /** Return the ClassType of the Procedure this is being compiled into. */
  protected ClassType getCompiledClassType(Compilation comp)
  {
    if (type == Compilation.typeProcedure)
      throw new Error("internal error: getCompiledClassType");
    return type;
  }

  public Type getType()
  {
    return type;
  }

  public void setType (ClassType type)
  {
    this.type = type;
  }

  /** Number of argument variable actually passed by the caller.
   * For functions that accept more than 4 argument, or take a variable number,
   * this is 1, since in that all arguments are passed in a single array. */
  public int incomingArgs ()
  {
    // The max_args > 0 is a hack to handle LambdaProcedure, which
    // currently always uses a single array argument.
    return min_args == max_args && max_args <= 4 && max_args > 0 ? max_args : 1;
  }

  /** If non-zero, the selector field of the ModuleMethod for this. */
  int selectorValue;

  int getSelectorValue(Compilation comp)
  {
    int s = selectorValue;
    if (s == 0)
      {
	s = comp.maxSelectorValue;
	comp.maxSelectorValue = s + primMethods.length;
	selectorValue = ++s;
      }
    return s;
  }

  /** Methods used to implement this functions.
   * primMethods[0] is used if the argument count is min_args;
   * primMethods[1] is used if the argument count is min_args+1;
   * primMethods[primMethods.length-1] is used otherwise.
   */
  Method[] primMethods;

  /** Select the method used given an argument count. */
  public final Method getMethod(int argCount)
  {
    if (primMethods == null || (max_args >= 0 && argCount > max_args))
      return null;
    int index = argCount - min_args;
    if (index < 0)
      return null; // Too few arguments.
    int length = primMethods.length;
    return primMethods[index < length ? index : length - 1];
  }

  /** Get the method that contains the actual body of the procedure.
   * (The other methods are just stubs that call that method.) */
  public final Method getMainMethod()
  {
    Method[] methods = primMethods;
    return methods == null ? null : methods[methods.length-1];
  }

  /** Return the parameter type of the "keyword/rest" parameters. */
  public final Type restArgType()
  {
    if (min_args == max_args)
      return null;
    if (primMethods == null)
      throw new Error("internal error - restArgType");
    Method[] methods = primMethods;
    if (max_args >= 0 && methods.length > max_args - min_args)
      return null;
    Method method = methods[methods.length-1];
    Type[] types = method.getParameterTypes();
    int ilast = types.length-1;
    if (method.getName().endsWith("$X"))
      ilast--;
    return types[ilast];
  }

  public LambdaExp outerLambda ()
  {
    return outer == null ? null : outer.currentLambda ();
  }

  /** Return the closest outer non-inlined LambdaExp. */

  public LambdaExp outerLambdaNotInline ()
  {
    for (ScopeExp exp = this; (exp = exp.outer) != null; )
      {
	if (exp instanceof LambdaExp)
	  {
	    LambdaExp result = (LambdaExp) exp;
	    if (! result.getInlineOnly())
	      return result;
	  }
      }
    return null;
  }

  /** True if given LambdaExp is inlined in this function, perhaps indirectly.
   * Is false if this is not inline-only or if getCaller() is not inlined is
   * outer.  Usually the same as (this.outerLambdaNotInline()==outer),
   * except in the case that outer.getInlineOnly(). */
  boolean inlinedIn (LambdaExp outer)
  {
    if (! getInlineOnly())
      return false;
    for (ScopeExp exp = getCaller(); exp != null;  exp = exp.outer)
      {
	if (exp instanceof LambdaExp)
	  {
	    
	    LambdaExp result = (LambdaExp) exp;
	    if (result == outer)
	      return true;
	    if (! result.getInlineOnly())
	      return false;
	  }
      }
    return false;
  }

  /** For an INLINE_ONLY function, return the function it gets inlined in. */
  public LambdaExp getCaller ()
  {
    return returnContinuation.context;
  }

  Variable thisVariable;

  public Variable declareThis(ClassType clas)
  {
    if (thisVariable == null)
      {
        thisVariable = new Variable("this");
	getVarScope().addVariableAfter(null, thisVariable);
	thisVariable.setParameter (true);
      }
    if (thisVariable.getType() == null)
      thisVariable.setType(clas);
    if (decls != null && decls.isThisParameter())
      decls.var = thisVariable;
    return thisVariable;
  }

  public Variable declareClosureEnv()
  {
    if (closureEnv == null && getNeedsClosureEnv())
      {
	LambdaExp parent = outerLambda();
	if (parent instanceof ClassExp)
	  parent = parent.outerLambda();
	Variable parentFrame = parent.heapFrame != null ?  parent.heapFrame
	  : parent.closureEnv;
	if (isClassMethod())
	  closureEnv = declareThis(type);
	else if (parent.heapFrame == null && ! parent.getNeedsStaticLink()
		 && ! (parent instanceof ModuleExp))
	  closureEnv = null;
	else if (! isClassGenerated() && ! getInlineOnly())
	  {
	    Method primMethod = getMainMethod();
	    if (! primMethod.getStaticFlag())
	      closureEnv = declareThis(primMethod.getDeclaringClass());
	    else
	      {
		Type envType = primMethod.getParameterTypes()[0];
		closureEnv = new Variable("closureEnv", envType);
		getVarScope().addVariableAfter(null, closureEnv);
		closureEnv.setParameter(true);
	      }
	  }
	else if (inlinedIn(parent))
	  closureEnv = parentFrame;
	else
	  {
	    closureEnv = new Variable("closureEnv", parentFrame.getType());
	    getVarScope().addVariable(closureEnv);
	  }
      }
    return closureEnv;
  }

  public LambdaExp ()
  {
  }

  public LambdaExp(int args)
  {
    min_args = args;
    max_args = args;
  }


  public LambdaExp (Expression body)
  {
    this.body = body;
  }

  /** Generate code to load heapFrame on the JVM stack. */
  public void loadHeapFrame (Compilation comp)
  {
    LambdaExp curLambda = comp.curLambda;
    while (curLambda != this && curLambda.getInlineOnly())
      curLambda = curLambda.getCaller();

    gnu.bytecode.CodeAttr code = comp.getCode();
    if (curLambda.heapFrame != null && this == curLambda)
      {
        code.emitLoad(curLambda.heapFrame);
        return;
      }
    ClassType curType;
    if (curLambda.closureEnv != null)
      {
        code.emitLoad(curLambda.closureEnv);
        curType = (ClassType) curLambda.closureEnv.getType();
      }
    else
      {
        code.emitPushThis();
        curType = comp.curClass;
      }
    while (curLambda != this)
      {
        Field link = curLambda.staticLinkField;
        if (link != null && link.getDeclaringClass() == curType)
          {
            code.emitGetField(link);
            curType = (ClassType) link.getType();
          }
        curLambda = curLambda.outerLambda();
      }
  }

  /** Get the i'the formal parameter. */
  Declaration getArg (int i)
  {
    for (Declaration var = firstDecl();  ; var = var.nextDecl ())
      {
	if (var == null)
	  throw new Error ("internal error - getArg");
        if (i == 0)
          return var;
        --i;
      }
  }

  public void compileEnd (Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (! getInlineOnly())
      {
	if (comp.method.reachableHere()
	    && (Compilation.defaultCallConvention < Compilation.CALL_WITH_TAILCALLS
		|| isModuleBody() || isClassMethod() || isHandlingTailCalls()))
	  code.emitReturn();
	popScope(code);        // Undoes enterScope in allocParameters
	if (! Compilation.fewerClasses) // FIXME
	  code.popScope(); // Undoes pushScope in method.initCode.
      }

    if (heapFrame != null)
      comp.generateConstructor((ClassType) heapFrame.getType(), this);
    
    generateApplyMethods(comp);
  }

  public void generateApplyMethods(Compilation comp)
  {
    comp.generateMatchMethods(this);
    int numApplyMethods
      = applyMethods == null ? 0 : applyMethods.size();
    if (Compilation.defaultCallConvention >= Compilation.CALL_WITH_CONSUMER)
      comp.generateApplyMethodsWithContext(this);
    else
      comp.generateApplyMethodsWithoutContext(this);
  }

  Field allocFieldFor (Compilation comp)
  {
    if (nameDecl != null && nameDecl.field != null)
      return nameDecl.field;
    String name = getName();
    String fname
      = name == null ? "lambda" : Compilation.mangleNameIfNeeded(name);
    int fflags = Access.FINAL;
    if (nameDecl != null && nameDecl.context instanceof ModuleExp)
      {
	boolean external_access = nameDecl.needsExternalAccess();
	if (external_access)
	  fname = Declaration.PRIVATE_PREFIX + fname;
	if (nameDecl.getFlag(Declaration.STATIC_SPECIFIED))
          {
            fflags |= Access.STATIC;
            // If there is no moduleInstanceVar, then the field gets
	    // initialized in <init>, not <clinit>,
	    // which is bad for a "static final" field.
            if (! ((ModuleExp) nameDecl.context).isStatic())
              fflags &= ~Access.FINAL;
          }
	if (! nameDecl.isPrivate() || external_access)
	  fflags |= Access.PUBLIC;
      }
    else
      {
	fname = fname + "$Fn" + ++comp.localFieldIndex;
	if (! getNeedsClosureEnv())
	  fflags = (fflags | Access.STATIC) & ~Access.FINAL;
      }
    ClassType frameType = getOwningLambda().getHeapFrameType();
    Type rtype = Compilation.typeModuleMethod;
    Field field = frameType.addField (fname, rtype, fflags);
    if (nameDecl != null)
      nameDecl.field = field;
    return field;
  }

  final void addApplyMethod (Compilation comp)
  {
    LambdaExp owner = this;
    // Similar to getOwningLambda(), but we can't add apply methods
    // to a ClassExp - at least not unless it extends ModuleBody.
    for (;;)
      {
        owner = owner.outerLambda();
	if (owner instanceof ModuleExp
	    || owner.heapFrame != null)
          break;
      }
    ClassType frameType = owner.getHeapFrameType();
    if (! (frameType.getSuperclass().isSubtype(Compilation.typeModuleBody)))
      owner = comp.getModule();
    if (owner.applyMethods == null)
      owner.applyMethods = new Vector();
    owner.applyMethods.addElement(this);
  }

  public Field compileSetField (Compilation comp)
  {
    if (comp.usingCPStyle())
      compile(comp, Type.pointer_type);
    else
      {
	compileAsMethod(comp);
	addApplyMethod(comp);
      }

    return (new ProcInitializer(this, comp)).field;
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget
	&& (getInlineOnly() || ! getCanRead()))
      return;
    Type rtype;
    CodeAttr code = comp.getCode();

    if (comp.usingCPStyle())
      {
	//	Label func_start = new Label(code);
	Label func_end = new Label(code);
	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = this;
	type = saveLambda.type;
	closureEnv = saveLambda.closureEnv;
        /*
	if (comp.usingCPStyle())
	  {
	    heapFrame = comp.thisDecl;
	    for (Declaration var = firstDecl();
		 var != null; var = var.nextDecl())
	      var.assignField(comp);
	  }
        */
	gnu.bytecode.SwitchState fswitch = comp.fswitch;
	int pc = comp.fswitch.getMaxValue() + 1;
	code.emitGoto(func_end);
	Type[] stackTypes = code.saveStackTypeState(true);

	fswitch.addCase(pc, code);
        /*
	code.emitPushThis();
	code.emitGetField(comp.argsCallContextField);
	code.emitStore(comp.argsArray);
        */
	allocParameters(comp);
	enterFunction(comp);

	compileBody(comp);
	compileEnd(comp);
	comp.curLambda = saveLambda;
	func_end.define(code);
	code.restoreStackTypeState(stackTypes);
	ClassType ctype = comp.curClass;
	rtype = ctype;
	/*
	code.emitNew(ctype);
	code.emitDup(ctype);
	code.emitInvokeSpecial(ctype.constructor);
	code.emitDup(ctype);
	code.emitPushInt(pc);
	code.emitPutField(comp.saved_pcCallFrameField);
	if (isHandlingTailCalls())
	  {
	    // Set name field.
	    if (name != null)
	      {
		code.emitDup(ctype);
		code.emitPushString(name);
		code.emitInvokeVirtual(comp.setNameMethod);
	      }
	    // Set numArgs field.
	    code.emitDup(ctype);
	    code.emitPushInt(min_args | (max_args << 12));
	    code.emitPutField(comp.numArgsCallFrameField);
	    // Set static link field to this CallFrame.
	    code.emitDup(ctype);
	    code.emitPushThis();
	    code.emitPutField(comp.callerCallFrameField);
	  }
	*/
      }
    else
      { LambdaExp outer = outerLambda();
	rtype = Compilation.typeModuleMethod;
	if ((flags & NO_FIELD) != 0
	    || (comp.immediate && outer instanceof ModuleExp))
	  {
	    compileAsMethod(comp);
	    addApplyMethod(comp);
	    ProcInitializer.emitLoadModuleMethod(this, comp);
	  }
	else
	  {
	    Field field = compileSetField(comp);
	    if (field.getStaticFlag())
	      code.emitGetStatic(field);
	    else
	      {
		LambdaExp parent = comp.curLambda;
		Variable frame
		  = parent.heapFrame != null ? parent.heapFrame
		  : parent.closureEnv;
		code.emitLoad(frame);
		code.emitGetField(field);
	      }
	  }
      }
    target.compileFromStack(comp, rtype);
  }

  public ClassType getHeapFrameType()
  {
    if (this instanceof ModuleExp || this instanceof ClassExp)
      return (ClassType) getType();
    else
      return (ClassType) heapFrame.getType();
  }


  public LambdaExp getOwningLambda()
  {
    ScopeExp exp = outer;
    for (;; exp = exp.outer)
      {
	if (exp == null)
	  return null;
	if (exp instanceof ModuleExp
	    || (exp instanceof ClassExp && getNeedsClosureEnv())
	    || (exp instanceof LambdaExp
		&& ((LambdaExp) exp).heapFrame != null))
	  return (LambdaExp) exp;
      }
  }

  void addMethodFor (Compilation comp, ObjectType closureEnvType)
  {
    ScopeExp sc = this;
    while (sc != null && ! (sc instanceof ClassExp))
      sc = sc.outer;
    ClassType ctype;
    // If this is nested inside a Class, then create the method in that
    // class - in case it references a private field/method.
    if (sc != null)
      ctype = ((ClassExp) sc).instanceType;
    else
      ctype = getOwningLambda().getHeapFrameType();
    addMethodFor(ctype, comp, closureEnvType);
  }

  void addMethodFor (ClassType ctype, Compilation comp, ObjectType closureEnvType)
  {
    // generate_unique_name (new_class, child.getName());
    String name = getName();
    LambdaExp outer = outerLambda();

    int key_args = keywords == null ? 0 : keywords.length;
    int opt_args = defaultArgs == null ? 0 : defaultArgs.length - key_args;
    int numStubs =
      ((flags & DEFAULT_CAPTURES_ARG) != 0) ? 0 : opt_args;
    boolean varArgs = max_args < 0 || min_args + numStubs < max_args;
    primMethods = new Method[numStubs + 1];

    boolean isStatic;
    // 'I' if initMethod ($finit$); 'C' if clinitMethod (<clinit>).
    char isInitMethod = '\0';
    if (nameDecl != null
	&& nameDecl.getFlag(Declaration.NONSTATIC_SPECIFIED))
      isStatic = false;
    else if (nameDecl != null
	     && nameDecl.getFlag(Declaration.STATIC_SPECIFIED))
      isStatic = true;
    else if (isClassMethod())
      {
	if (outer instanceof ClassExp)
	  {
	    ClassExp cl = (ClassExp) outer;
	    isStatic = cl.isMakingClassPair() && closureEnvType != null;
	    if (this == cl.initMethod)
	      isInitMethod = 'I';
	    else if (this == cl.clinitMethod)
	      {
		isInitMethod = 'C';
		isStatic = true;
	      }
	  }
	else
	  isStatic = false;
      }
    else if (thisVariable != null || closureEnvType == ctype)
      isStatic = false;
    else if (nameDecl != null && nameDecl.context instanceof ModuleExp)
      {
	ModuleExp mexp = (ModuleExp) nameDecl.context;
	isStatic = mexp.getSuperType() == null && mexp.getInterfaces() == null;
      }
    else
      isStatic = true;

    StringBuffer nameBuf = new StringBuffer(60);
    int mflags = isStatic ? Access.STATIC : 0;
    if (nameDecl != null)
      {
	if (nameDecl.needsExternalAccess())
	  mflags |=  Access.PUBLIC;
	else
          {
            short defaultFlag = nameDecl.isPrivate() ? 0 : Access.PUBLIC;
            if (isClassMethod())
              defaultFlag = nameDecl.getAccessFlags(defaultFlag);
            mflags |= defaultFlag;
          }
      }
    if (! (outer.isModuleBody() || outer instanceof ClassExp)
	|| name == null)
      {
	nameBuf.append("lambda");
	nameBuf.append(+(++comp.method_counter));
      }
    if (isInitMethod == 'C')
      nameBuf.append("<clinit>");
    else if (getSymbol() != null)
      nameBuf.append(Compilation.mangleName(name));
    if (getFlag(SEQUENCE_RESULT))
      nameBuf.append("$C");
    boolean withContext
      = (getCallConvention() >= Compilation.CALL_WITH_CONSUMER
	 && isInitMethod == '\0');
    if (isInitMethod != '\0')
      {
	if (isStatic)
	  { // if cl.isMakingClassPair() - i.e. defining a non-simple class:
	    // In this case the $finit$ method needs to be explicitly called
	    // by sub-class constructors.  See Compilation.callInitMethods.
	    mflags = (mflags & ~Access.PROTECTED+Access.PRIVATE)+Access.PUBLIC;
	  }
	else
	  { // if ! cl.isMakingClassPair() - i.e. defining a simple class:
	    // Make it private to prevent inherited $finit$ from overriding
	    // the current one - and thus preventing its execution.
	    mflags = (mflags & ~Access.PUBLIC+Access.PROTECTED)+Access.PRIVATE;
	  }
      }
    if (ctype.isInterface())
      mflags |= Access.ABSTRACT;
    if (! isStatic)
      declareThis(ctype);

    Type rtype
      = (getFlag(SEQUENCE_RESULT)
	 || getCallConvention () >= Compilation.CALL_WITH_CONSUMER)
      ? Type.void_type
      : getReturnType().getImplementationType();
    int extraArg = (closureEnvType != null && closureEnvType != ctype) ? 1 : 0;

    int ctxArg = 0;
    if (getCallConvention () >= Compilation.CALL_WITH_CONSUMER
	&& isInitMethod == '\0')
      ctxArg = 1;

    int nameBaseLength = nameBuf.length();
    for (int i = 0;  i <= numStubs;  i++)
      {
	nameBuf.setLength(nameBaseLength);
	int plainArgs = min_args + i;
	int numArgs = plainArgs;
	if (i == numStubs && varArgs)
	  numArgs++;
	Type[] atypes = new Type[extraArg + numArgs + ctxArg];
	if (extraArg > 0)
	  atypes[0] = closureEnvType;
	Declaration var = firstDecl();
        if (var != null && var.isThisParameter())
          var = var.nextDecl();
	for (int itype = 0; itype < plainArgs; var = var.nextDecl())
	  atypes[extraArg + itype++] = var.getType().getImplementationType();
	if (ctxArg != 0)
	  atypes[atypes.length-1] = Compilation.typeCallContext;
	if (plainArgs < numArgs)
	  {
	    nameBuf.append("$V");
	    name = nameBuf.toString();
	    Type lastType = var.getType();
	    String lastTypeName = lastType.getName();
	    if (key_args > 0 || numStubs < opt_args
		|| ! ("gnu.lists.LList".equals(lastTypeName)
		      || "java.lang.Object[]".equals(lastTypeName)))
	      {
		lastType = Compilation.objArrayType;
		argsArray = new Variable("argsArray",
					 Compilation.objArrayType);
		argsArray.setParameter(true);
	      }
	    firstArgsArrayArg = var;
	    atypes[atypes.length-(withContext ? 2 : 1)] = lastType;
	  }
	if (withContext)
	  nameBuf.append("$X");

	boolean classSpecified
	  = (outer instanceof ClassExp
	     || (outer instanceof ModuleExp
		 && (((ModuleExp) outer)
		     .getFlag(ModuleExp.SUPERTYPE_SPECIFIED))));
	name = nameBuf.toString();
	{
	  // Rename the method if an existing method has the same
	  // name and type in this class.
	  // Additionally, if the base class or interfaces were not explicitly
	  // specified, then search super-classes for conflicting methods
	  // (such as "run" or "apply").
	  int renameCount = 0;
	  int len = nameBuf.length();
	retry:
	  for (;;)
	    {
	      for (ClassType t = ctype;  t != null; t = t.getSuperclass ())
		{
		  if (t.getDeclaredMethod(name, atypes) != null)
		    {
		      nameBuf.setLength(len);
		      nameBuf.append('$');
		      nameBuf.append(++renameCount);
		      name = nameBuf.toString();
		      continue retry;
		    }
		  if (classSpecified)
		    // Do not search in super-classes
		    break;
		}
	      break;
	    }
	}
	Method method = ctype.addMethod(name, atypes, rtype, mflags);
	primMethods[i] = method;

	if (throwsSpecification != null && throwsSpecification.length > 0)
	  {
	    int n = throwsSpecification.length;
	    ClassType[] exceptions = new ClassType[n];
	    for (int j = 0;  j < n;  j++)
	      {
		ClassType exception = null;
		Declaration decl = throwsSpecification[j].getBinding();
		if (decl != null)
		  {
		    Expression declValue = decl.getValue();
		    if (declValue instanceof ClassExp)
		      exception
			= ((ClassExp) declValue).getCompiledClassType(comp);
		    else
		      comp.error('e', "throws specification "+decl.getName()
				 + " has non-class lexical binding");
		  }
		if (exception == null)
		  {
		    String exName = throwsSpecification[j].getName();
		    int nlen = exName.length();
		    if (nlen > 2
			&& exName.charAt(0) == '<'
			&& exName.charAt(nlen-1) == '>')
		      exName = exName.substring(1, nlen-1);
		    exception = ClassType.make(exName);
		  }
		exceptions[j] = exception;
	      }
	    ExceptionsAttr attr = new ExceptionsAttr(method);
	    attr.setExceptions(exceptions);
	  }
      }
  }

  // Can we merge this with allocParameters?
  public void allocChildClasses (Compilation comp)
  {
    if (this instanceof ModuleExp)
      ((ModuleExp) this).allocFields(comp);
    else
      {
	Method main = getMainMethod();
	
	Declaration decl = firstDecl();
	for (;;)
	  {
	    if (decl == firstArgsArrayArg && argsArray != null)
	      {
		getVarScope().addVariable(argsArray);
	      } 
	    if (! getInlineOnly()
		&& getCallConvention() >= Compilation.CALL_WITH_CONSUMER
		&& (firstArgsArrayArg == null ? decl == null
		    : argsArray != null ? decl == firstArgsArrayArg
		    : decl == firstArgsArrayArg.nextDecl()))
	      {
		Variable var =
		  getVarScope().addVariable(null,
					    Compilation.typeCallContext,
					    "$ctx");
		var.setParameter(true);
	      } 
	    if (decl == null)
	      break;
	    Variable var = decl.var;
	    // i is the register to use for the current parameter
            if (var != null)
              ;
	    else if (decl.isSimple () && ! decl.isIndirectBinding())
	      {
		// For a simple parameter not captured by an inferior lambda,
		// just allocate it in the incoming register.
                var = decl.allocateVariable(null);
		//var.allocateLocal(code);
	      }
	    else
	      {
		// This variable was captured by an inner lambda.
		// Its home location is in the heapFrame.
		// Later, we copy it from its incoming register
		// to its home location heapFrame.  Here we just create and
		// assign a Variable for the incoming (register) value.
		String vname
                  = Compilation.mangleName(decl.getName()).intern();
		Type vtype = decl.getType().getImplementationType();
                var = decl.var = getVarScope().addVariable(null, vtype, vname);
		//getVarScope().addVariableAfter(var, decl);
		var.setParameter (true);
		//var.allocateLocal(code);
	      }
	    decl = decl.nextDecl();
	  }
      }

    declareClosureEnv();

    if (comp.usingCPStyle() && comp.curClass == comp.mainClass)
      return;

    allocFrame(comp);

    for (LambdaExp child = firstChild;  child != null;
	 child = child.nextSibling)
      {
	if (! child.isClassGenerated() && ! child.getInlineOnly())
	  {
	    ObjectType closureEnvType;
	    if (! child.getNeedsClosureEnv())
	      closureEnvType = null;
	    else if (this instanceof ClassExp)
	      closureEnvType = getCompiledClassType(comp);
            else
              {
                LambdaExp owner = this;
                while (owner.heapFrame == null)
		  owner = owner.outerLambda();
                closureEnvType = (ClassType) owner.heapFrame.getType();
              }
	    child.addMethodFor(comp, closureEnvType);
	  }
      }
  }

  public void allocFrame (Compilation comp)
  {
    if (heapFrame != null)
      {
	ClassType frameType;
	if (this instanceof ModuleExp || this instanceof ClassExp)
	  frameType = getCompiledClassType(comp);
	else
	  {
	    frameType = new ClassType(comp.generateClassName("frame"));
	    frameType.setSuper(comp.getModuleType());
	    comp.addClass(frameType);
	  }
	heapFrame.setType(frameType);
      }
  }

  void allocParameters (Compilation comp)
  {
    CodeAttr code = comp.getCode();

    int i = 0;
    int j = 0;

    code.locals.enterScope(getVarScope());
    int line = getLine();
    if (line != 0)
      code.putLineNumber(getFile(), line);

    for (Declaration decl = firstDecl();  decl != null;  )
      {
        Variable var = decl.var;
	// If the only reason we are using an argsArray is because there
	// are more than 4 arguments, copy the arguments in local register.
	// Then forget about the args array.  We do this first, before
	// the label that tail-recursion branches back to.
	// This way, a self-tail-call knows where to leave the argumnents.
	if (argsArray != null && min_args == max_args
	    && primMethods == null
	    && getCallConvention() < Compilation.CALL_WITH_CONSUMER)
	  {
	    code.emitLoad(argsArray);
	    code.emitPushInt(j);
	    code.emitArrayLoad(Type.pointer_type);
	    decl.getType().emitCoerceFromObject(code);
	    code.emitStore(decl.getVariable());
	  }
	j++;
	i++;
	decl = decl.nextDecl();
      }
    if (heapFrame != null)
      heapFrame.allocateLocal(code);
  }

  static Method searchForKeywordMethod3;
  static Method searchForKeywordMethod4;

  /** Rembembers stuff to do in <init> of this class. */
  Initializer initChain;

  void enterFunction (Compilation comp)
  {
    CodeAttr code = comp.getCode();

    // Tail-calls loop back to here!
    getVarScope().setStartPC(code);

    if (closureEnv != null && ! closureEnv.isParameter()
	&& ! comp.usingCPStyle())
      {
	if (! getInlineOnly())
	  {
	    code.emitPushThis();
	    Field field = closureEnvField;
	    if (field == null)
	      field = outerLambda().closureEnvField;
	    code.emitGetField(field);
	    code.emitStore(closureEnv);
	  }
	else if (! inlinedIn(outerLambda()))
	  {
	    outerLambda().loadHeapFrame(comp);
	    code.emitStore(closureEnv);
	  }
      }
    if (! comp.usingCPStyle())
      {
	int fflags;
	ClassType frameType;
	if (heapFrame == null)
	  {
	    fflags = Access.STATIC;
	    frameType = currentModule().getCompiledClassType(comp);
	  }
	else
	  {
	    fflags = 0;
	    frameType = (ClassType) heapFrame.getType();
	  }
	for (Declaration decl = capturedVars; decl != null;
	     decl = decl.nextCapturedVar)
	  {
	    if (decl.field != null)
	      continue;
            decl.makeField(frameType, comp, null);
	  }
      }
    if (heapFrame != null && ! comp.usingCPStyle())
      {
	ClassType frameType = (ClassType) heapFrame.getType();
	if (closureEnv != null && ! (this instanceof ModuleExp))
	  staticLinkField = frameType.addField("staticLink",
					       closureEnv.getType());
        if (! (this instanceof ModuleExp) && ! (this instanceof ClassExp))
          {
	    code.emitNew(frameType);
	    code.emitDup(frameType);
	    Method constructor = Compilation.getConstructor(frameType, this);
	    code.emitInvokeSpecial(constructor);

            if (staticLinkField != null)
              {
                code.emitDup(heapFrame.getType());
                code.emitLoad(closureEnv);
                code.emitPutField(staticLinkField);
              }
            code.emitStore(heapFrame);
          }
      }

    Variable argsArray = this.argsArray;
    if (min_args == max_args && ! Compilation.fewerClasses
	&& primMethods == null
	&& getCallConvention () < Compilation.CALL_WITH_CONSUMER)
      argsArray = null;

    // For each non-artificial parameter, copy it from its incoming
    // location (a local variable register, or the argsArray) into
    // its home location, if they are different.
    int i = 0;
    int opt_i = 0;
    int key_i = 0;
    int key_args = keywords == null ? 0 : keywords.length;
    int opt_args = defaultArgs == null ? 0
      : defaultArgs.length - key_args;
    if (this instanceof ModuleExp)
      return;
    // If plainArgs>=0, it is the number of arguments *not* in argsArray.
    int plainArgs = -1;
    int defaultStart = 0;
    Method mainMethod = getMainMethod();
    Variable callContextSave = comp.callContextVar;

    for (Declaration param = firstDecl();  param != null; param = param.nextDecl())
      {
        comp.callContextVar
          = (getCallConvention() < Compilation.CALL_WITH_CONSUMER ? null
             : getVarScope().lookup("$ctx"));
	if (param == firstArgsArrayArg && argsArray != null)
	  {
	    if (primMethods != null)
	      {
		plainArgs = i;
		defaultStart = plainArgs - min_args;
	      }
	    else
	      {
		plainArgs = 0;
		defaultStart = 0;
	      }
	  }
	if (plainArgs >= 0 || ! param.isSimple()
	    || param.isIndirectBinding())
	  {
	    Type paramType = param.getType();
	    Type stackType
	      = (mainMethod == null || plainArgs >= 0 ? Type.pointer_type
		 : paramType);
	    // If the parameter is captured by an inferior lambda,
	    // then the incoming parameter needs to be copied into its
	    // slot in the heapFrame.  Thus we emit an aaload instruction.
	    // Unfortunately, it expects the new value *last*,
	    // so first push the heapFrame array and the array index.
	    if (!param.isSimple ())
	      param.loadOwningObject(null, comp);
	    // This part of the code pushes the incoming argument.
	    if (plainArgs < 0)
	      {
		// Simple case:  Use Incoming register.
		code.emitLoad(param.getVariable());
	      }
            else if (i < min_args)
	      { // This is a required parameter, in argsArray[i].
		code.emitLoad(argsArray);
		code.emitPushInt(i);
		code.emitArrayLoad(Type.pointer_type);
	      }
            else if (i < min_args + opt_args)
	      { // An optional parameter
		code.emitPushInt(i - plainArgs);
                code.emitLoad(argsArray);
		code.emitArrayLength();
		code.emitIfIntLt();
                code.emitLoad(argsArray);
		code.emitPushInt(i - plainArgs);
		code.emitArrayLoad(Type.pointer_type);
		code.emitElse();
		defaultArgs[defaultStart + opt_i++].compile(comp, paramType);
		code.emitFi();
	      }
	    else if (max_args < 0 && i == min_args + opt_args)
	      {
		// This is the "rest" parameter (i.e. following a "."):
		// Convert argsArray[i .. ] to a list.
		code.emitLoad(argsArray);
		code.emitPushInt(i - plainArgs);
		code.emitInvokeStatic(Compilation.makeListMethod);
		stackType = Compilation.scmListType;
              }
	    else
	      { // Keyword argument.
		code.emitLoad(argsArray);
		code.emitPushInt(min_args + opt_args - plainArgs);
		comp.compileConstant(keywords[key_i++]);
		Expression defaultArg = defaultArgs[defaultStart + opt_i++];
		// We can generate better code if the defaultArg expression
		// has no side effects.  For simplicity and safety, we just
		// special case literals, which handles most cases.
		if (defaultArg instanceof QuoteExp)
		  {
		    if (searchForKeywordMethod4 == null)
		      {
			Type[] argts = new Type[4];
			argts[0] = Compilation.objArrayType;
			argts[1] = Type.int_type;
			argts[2] = Type.pointer_type;
			argts[3] = Type.pointer_type;
			searchForKeywordMethod4
			  = Compilation.scmKeywordType.addMethod
			  ("searchForKeyword",  argts,
			   Type.pointer_type, Access.PUBLIC|Access.STATIC);
		      }
		    defaultArg.compile(comp, paramType);
		    code.emitInvokeStatic(searchForKeywordMethod4);
		  }
		else
		  {
		    if (searchForKeywordMethod3 == null)
		      {
			Type[] argts = new Type[3];
			argts[0] = Compilation.objArrayType;
			argts[1] = Type.int_type;
			argts[2] = Type.pointer_type;
			searchForKeywordMethod3
			  = Compilation.scmKeywordType.addMethod
			  ("searchForKeyword",  argts,
			   Type.pointer_type, Access.PUBLIC|Access.STATIC);
		      }
		    code.emitInvokeStatic(searchForKeywordMethod3);
		    code.emitDup(1);
		    comp.compileConstant(Special.dfault);
		    code.emitIfEq();
		    code.emitPop(1);
		    defaultArg.compile(comp, paramType);
		    code.emitFi();
		  }
	      }
	    // Now finish copying the incoming argument into its
	    // home location.
            if (paramType != stackType)
	      CheckedTarget.emitCheckedCoerce(comp, this, i+1, paramType);
	    if (param.isIndirectBinding())
              param.pushIndirectBinding(comp);
	    if (param.isSimple())
	      code.emitStore(param.getVariable());
            else
	      code.emitPutField(param.field);
	  }
	i++;
      }
    comp.callContextVar = callContextSave;
  }

  void compileChildMethods (Compilation comp)
  {
    for (LambdaExp child = firstChild;  child != null; )
      {
	if (! child.getCanRead() && ! child.getInlineOnly())
	  {
	    child.compileAsMethod(comp);
	  }
	child = child.nextSibling;
      }
  }

  void compileAsMethod (Compilation comp)
  {
    if ((flags & METHODS_COMPILED) != 0)
      return;
    flags |= METHODS_COMPILED;
    Method save_method = comp.method;
    LambdaExp save_lambda = comp.curLambda;
    comp.curLambda = this;

    Method method = primMethods[0];
    boolean isStatic = method.getStaticFlag();
    int numStubs = primMethods.length - 1;
    Type restArgType = restArgType();

    int[] saveDeclFlags = null;
    if (numStubs > 0)
      {
	saveDeclFlags = new int[min_args + numStubs];
	int k = 0;
	for (Declaration decl = firstDecl();
	     k < min_args + numStubs; decl = decl.nextDecl())
	  saveDeclFlags[k++] = decl.flags;
      }

    boolean ctxArg = getCallConvention () >= Compilation.CALL_WITH_CONSUMER;

    for (int i = 0;  i <= numStubs;  i++)
      {
	comp.method = primMethods[i];
	if (i < numStubs)
	  {
	    CodeAttr code = comp.method.startCode();
	    int toCall = i + 1;
	    while (toCall < numStubs
		   && defaultArgs[toCall] instanceof QuoteExp)
	      toCall++;
	    int thisArg = isStatic ? 0 : 1;
	    boolean varArgs = toCall == numStubs && restArgType != null;
	    Declaration decl;
            Variable callContextSave = comp.callContextVar;
	    Variable var = code.getArg(0);
	    if (! isStatic)
	      {
		code.emitPushThis();
		if (getNeedsClosureEnv())
		  closureEnv = var;
		var = code.getArg(1);
	      }
	    decl = firstDecl();
	    for (int j = 0;  j < min_args + i;  j++, decl = decl.nextDecl())
	      {
		decl.flags |= Declaration.IS_SIMPLE;
		decl.var = var;
		code.emitLoad(var);
		var = var.nextVar();
	      }
	    comp.callContextVar = ctxArg ? var : null;
	    for (int j = i; j < toCall;  j++, decl = decl.nextDecl())
	      {
		Target paramTarget = StackTarget.getInstance(decl.getType());
		defaultArgs[j].compile(comp, paramTarget);
	      }
	    if (varArgs)
	      {
		Expression arg;
		String lastTypeName = restArgType.getName();
		if ("gnu.lists.LList".equals(lastTypeName))
		  arg = new QuoteExp(gnu.lists.LList.Empty);
		else if ("java.lang.Object[]".equals(lastTypeName))
		  arg = new QuoteExp(Values.noArgs);
		else // FIXME
		  throw new Error("unimplemented #!rest type "+lastTypeName);
		arg.compile(comp, restArgType);
	      }
	    if (ctxArg)
	      code.emitLoad(var);
	    if (isStatic)
	      code.emitInvokeStatic(primMethods[toCall]);
	    else
	      code.emitInvokeVirtual(primMethods[toCall]);
	    code.emitReturn();
	    closureEnv = null;
            comp.callContextVar = callContextSave;
	  }
	else
	  {
	    if (saveDeclFlags != null)
	      {
		int k = 0;
		for (Declaration decl = firstDecl();
		     k < min_args + numStubs; decl = decl.nextDecl())
		  {
		    decl.flags = saveDeclFlags[k++];
		    decl.var = null;
		  }
	      }
	    comp.method.initCode();
	    allocChildClasses(comp);
	    allocParameters(comp);
	    enterFunction(comp);

	    compileBody(comp);
	    compileEnd(comp);
	  }
      }

    compileChildMethods(comp);
    comp.method = save_method;
    comp.curLambda = save_lambda;
  }

  public void compileBody (Compilation comp)
  {
    Target target;
    Variable callContextSave = comp.callContextVar;
    comp.callContextVar = null;
    if (getCallConvention() >= Compilation.CALL_WITH_CONSUMER)
      {
	Variable var = getVarScope().lookup("$ctx");
	if (var != null && var.getType() == Compilation.typeCallContext)
          comp.callContextVar = var;
        target = ConsumerTarget.makeContextTarget(comp);
      }
    else
      target = Target.pushValue(getReturnType());
    body.compileWithPosition(comp, target,
			     body.getLine() > 0 ? body : this);
    comp.callContextVar = callContextSave;
  }

  /** A cache if this has already been evaluated. */
  Procedure thisValue;

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkLambdaExp(this);
  }

  protected void walkChildren(ExpWalker walker)
  {
    walkChildrenOnly(walker);
    walkProperties(walker);
  }

  protected final void walkChildrenOnly(ExpWalker walker)
  {
    LambdaExp save = walker.currentLambda;
    walker.currentLambda = this;
    try
      {
	walker.walkDefaultArgs(this);
	if (walker.exitValue == null && body != null)
	  body = walker.walk(body);
      }
    finally
      {
	walker.currentLambda = save;
      }
  }

  protected final void walkProperties(ExpWalker walker)
  {
    if (properties != null)
      {
	int len = properties.length;
	for (int i = 1;  i < len;  i += 2)
	  {
	    Object val = properties[i];
	    if (val instanceof Expression)
	      {
		properties[i] = walker.walk((Expression) properties[i]);
	      }
	  }
      }
  }

  protected boolean mustCompile ()
  {
    if (keywords != null && keywords.length > 0)
      return true;
    if (defaultArgs != null)
      {
        for (int i = defaultArgs.length;  --i >= 0; )
          {
            Expression def = defaultArgs[i];
            // Non-constant default arguments require care with scoping.
            if (def != null && ! (def instanceof QuoteExp))
              return true;
          }
      }
    return false;
  }

  public void apply (CallContext ctx) throws Throwable
  {
    // It would be better to call setIndexes at compile-time, but that
    // doesn't work if we're called as a syntax expander at rewrite time.
    // Better, if this is a top-level eval, to create a "compile-time" module,
    // but I haven't figured out how to do that.  FIXME.
    setIndexes();
    ctx.writeValue(new Closure(this, ctx));
  }

  public Expression inline (ApplyExp exp, InlineCalls walker, Declaration decl)
  {
    int args_length = exp.args.length;
    String msg = WrongArguments.checkArgCount(getName(),
                                              min_args, max_args, args_length);
    if (msg != null)
      return walker.noteError(msg);
    int conv = getCallConvention();
    Compilation comp = walker.getCompilation();
    Method method;
    if (comp.inlineOk(this) && isClassMethod()
        && (conv <= Compilation.CALL_WITH_CONSUMER
            || (conv == Compilation.CALL_WITH_TAILCALLS))
        && (method = getMethod(args_length)) != null)
      {
        // This is an optimization to expand a call to a method in the
        // same ClassExp.  The result is a call to a PrimProcedure instead.
        // This isn't just an optimization, since the re-write is
        // needed to ensure that if we're in an inner lambda that the
        // $this$ declaration is captured in a closure.
        PrimProcedure mproc = new PrimProcedure(method, this);
        Expression[] margs;
        if (mproc.getStaticFlag())
          margs = exp.args;
        else
          {
            LambdaExp curLambda = walker.getCurrentLambda();
            for (;;)
              {
                if (curLambda == null)
                  return walker.noteError("internal error: missing "+this);
                if (curLambda.outer == outer) // I.e. same class.
                  break;
                curLambda = curLambda.outerLambda();
              }
            Declaration d = curLambda.firstDecl();
            if (d==null || ! d.isThisParameter())
              return walker.noteError("calling non-static method "
                                      +getName()+" from static method "
                                      +curLambda.getName());
            int nargs = exp.getArgCount();
            margs = new Expression[1 + nargs];
            System.arraycopy(exp.getArgs(), 0, margs, 1, nargs);
            margs[0] = new ThisExp(d);
          }
        ApplyExp nexp = new ApplyExp(mproc, margs);
        return nexp.setLine(exp);
      }
    return exp;
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Lambda/", ")", 2);
    Object sym = getSymbol();
    if (sym != null)
      {
	out.print(sym);
	out.print('/');
      }
    out.print(id);
    out.print('/');
    out.print("fl:");  out.print(Integer.toHexString(flags));
    out.writeSpaceFill();
    printLineColumn(out);
    out.startLogicalBlock("(", false, ")");
    Special prevMode = null;
    int i = 0;
    int opt_i = 0;
    int key_args = keywords == null ? 0 : keywords.length;
    int opt_args = defaultArgs == null ? 0 : defaultArgs.length - key_args;
    Declaration decl = firstDecl();
    if (decl != null && decl.isThisParameter())
      i = -1;
    for (; decl != null;  decl = decl.nextDecl())
      {
	Special mode;
	if (i < min_args)
	  mode = null;
	else if (i < min_args + opt_args)
	  mode = Special.optional;
	else if (max_args < 0 && i == min_args + opt_args)
	  mode = Special.rest;
	else
	  mode = Special.key;
	if (decl != firstDecl())
	   out.writeSpaceFill();
	if (mode != prevMode)
	  {
	    out.print(mode);
	    out.writeSpaceFill();
	  }
	Expression defaultArg = null;
	if (mode == Special.optional || mode == Special.key)
	  defaultArg = defaultArgs[opt_i++];
	if (defaultArg != null)
	  out.print('(');
	decl.printInfo(out);
	if (defaultArg != null && defaultArg != QuoteExp.falseExp)
	  {
	    out.print(' ');
	    defaultArg.print(out);
	    out.print(')');
	  }
	i++;
	prevMode = mode;
      }
    out.endLogicalBlock(")");
    out.writeSpaceLinear();
    if (body == null)
      out.print("<null body>");
    else
      body.print(out);
    out.endLogicalBlock(")");
  }

  protected final String getExpClassName()
  {
    String cname = getClass().getName();
    int index = cname.lastIndexOf('.');
    if (index >= 0)
      cname = cname.substring(index+1);
    return cname;
  }

  public String toString()
  {
    String str = getExpClassName()+':'+getSymbol()+'/'+id+'/';

	int l = getLine();
	if (l <= 0 && body != null)
	  l = body.getLine();
	if (l > 0)
	  str = str + "l:" + l;

    return str;
  }

  /** If non-null, a sequence of (key, value)-pairs.
   * These will be used to call setProperty at run-time. */
  Object[] properties;

  public Object getProperty(Object key, Object defaultValue)
  {
    if (properties != null)
      {
	for (int i = properties.length;  (i -= 2) >= 0; )
	  {
	    if (properties[i] == key)
	      return properties[i + 1];
	  }
      }
    return defaultValue;
  }

  public synchronized void setProperty(Object key, Object value)
  {
    properties = Procedure.setProperty(properties, key, value);
  }

  /** If non-null, the type of values returned by this function.
   * If null, the return type has not been set or calculated yet. */
  public Type returnType;

  /** The return type of this function, i.e the type of its returned values. */
  public final Type getReturnType()
  {
    if (returnType == null)
      {
	returnType = Type.pointer_type;  // To guards against cycles.
	// body may not be set if define scan'd but not yet rewrit'ten.
	if (body != null)
	  returnType = body.getType();
      }
    return returnType;
  }

  /* Set the return type of this function. */
  public final void setReturnType (Type returnType)
  {
    this.returnType = returnType;
  }
}

class Closure extends MethodProc
{
  Object[][] evalFrames;
  LambdaExp lambda;

  public int numArgs() { return lambda.min_args | (lambda.max_args << 12); }

  public Closure (LambdaExp lexp, CallContext ctx)
  {
    this.lambda = lexp;

    Object[][] oldFrames = ctx.evalFrames;
    if (oldFrames != null)
      {
        int n = oldFrames.length;
        while (n > 0 && oldFrames[n-1] == null)
          n--;

        evalFrames = new Object[n][];
        System.arraycopy(oldFrames, 0, evalFrames, 0, n);
      }
    setSymbol(lambda.getSymbol());
  }

  public int match0 (CallContext ctx)
  {
    return matchN(new Object[] { }, ctx);
  }

  public int match1 (Object arg1, CallContext ctx)
  {
    return matchN(new Object[] { arg1 }, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    return matchN(new Object[] { arg1, arg2 }, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    return matchN(new Object[] { arg1, arg2, arg3 }, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
                     CallContext ctx)
  {
    return matchN(new Object[] { arg1, arg2, arg3, arg4 }, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    int num = numArgs();
    int nargs = args.length;
    int min = num & 0xFFF;
    if (nargs < min)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    int max = num >> 12;
    if (nargs > max && max >= 0)
      return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;

    Object[] evalFrame = new Object[lambda.frameSize];
    int key_args = lambda.keywords == null ? 0 : lambda.keywords.length;
    int opt_args = lambda.defaultArgs == null ? 0
      : lambda.defaultArgs.length - key_args;
    int i = 0;
    int opt_i = 0;
    int min_args = lambda.min_args;
    for (Declaration decl = lambda.firstDecl(); decl != null;
         decl = decl.nextDecl())
      {
        Object value;
	if (i < min_args)
	  value = args[i++];
	else if (i < min_args + opt_args)
          {
            if (i < nargs)
              value = args[i++];
            else
              value = ((QuoteExp) lambda.defaultArgs[opt_i++]).getValue();
          }
	else if (lambda.max_args < 0 && i == min_args + opt_args)
          {
            if (decl.type instanceof ArrayType)
              {
                int rem = nargs - i;
                Type elementType = ((ArrayType) decl.type).getComponentType();
                if (elementType == Type.pointer_type)
                  {
                    Object[] rest = new Object[rem];
                    System.arraycopy(args, i, rest, 0, rem);
                    value = rest;
                  }
                else
                  {
                    Class elementClass = elementType.getReflectClass();
                    value
                      = java.lang.reflect.Array.newInstance(elementClass, rem);
                    for (int j = 0;  j < rem;  j++)
                      {
                        Object el;
                        try
                          {
                            el = elementType.coerceFromObject(args[i+j]);
                          }
                        catch (ClassCastException ex)
                          {
                            return NO_MATCH_BAD_TYPE|(i+j);
                          }
                        java.lang.reflect.Array.set(value, j, el);
                      }
                  }
              }
            else
              value = LList.makeList(args, i);
          }
        else // Should never happen
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
        if (decl.type != null)
          {
            try
              {
                value = decl.type.coerceFromObject(value);
              }
            catch (ClassCastException ex)
              {
                return NO_MATCH_BAD_TYPE|i;
              }
          }
        if (decl.isIndirectBinding())
          {
            gnu.mapping.Location loc = decl.makeIndirectLocationFor();
            loc.set(value);
            value = loc;
          }
        evalFrame[decl.evalIndex] = value;
      }
    ctx.values = evalFrame;
    ctx.where = 0;
    ctx.next = 0;
    ctx.proc = this;
    return 0; // FIXME
  }

  public void apply (CallContext ctx) throws Throwable
  {
    int level = ScopeExp.nesting(lambda);
    Object[] evalFrame = ctx.values;
    Object[][] saveFrames = ctx.evalFrames;

    int numFrames = evalFrames == null ? 0 : evalFrames.length;
    if (level >= numFrames)
      numFrames = level;
    numFrames += 10;
    Object[][] newFrames = new Object[numFrames][];
    if (evalFrames != null)
      System.arraycopy(evalFrames, 0, newFrames, 0, evalFrames.length);
    newFrames[level] = evalFrame;
    ctx.evalFrames = newFrames;

    try
      {
        lambda.body.apply(ctx);
      }
    finally
      {
        ctx.evalFrames = saveFrames;
      }
  }

  public Object getProperty(Object key, Object defaultValue)
  {
    Object value = super.getProperty(key, defaultValue);
    if (value == null)
      value = lambda.getProperty(key, defaultValue);
    return value;
  }
}
