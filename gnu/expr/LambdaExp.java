// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/**
 * Class used to implement Scheme lambda expressions.
 * @author	Per Bothner
 */

public class LambdaExp extends ScopeExp
{
  public String name;
  public Expression body;
  public int min_args;
  // Maximum number of actual arguments;  -1 if variable.
  public int max_args;

  //  public int plainArgs;
  Variable argsArray;
  // First argument that goes into argsArray.
  private Declaration firstArgsArrayArg;

  public Keyword[] keywords;
  public Expression[] defaultArgs;

  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  int id = ++counter;

  /** A list of Declarations, chained using Declaration's nextCapturedVar.
    * All the Declarations are allocated in the current heapFrame. */
  Declaration capturedVars;

  /** A local variable that points to the heap-allocated part of the frame.
   * This is an instance of heapFrameLambda (which is one of our children);
   * each captured variable is a field in the heapFrame.  A procedure has
   * a heapFrame iff if has a parameter or local variable that is
   * referenced ("captured") by a non-inline inferior procedure.
   * (I.e there is a least one non-inline procedure that encloses the
   * reference but not the definition.)  Note that an inline procedure may
   * have a heapFrame if it encloses a non-inline procedure.  This is
   * necessary because we represent loops as tail-recursive inline procedures.
   */
  Variable heapFrame;

  /** If any variables local to this LambdaExp are captured by some inner
   * non-lined Lambda, then all such variables are allocated in a heapFrame.
   * If (! usingCPStyle), then the heapFrame is an instance of the Procedure
   * compiled from one of our child lambdas, and heapFrameLambda points to
   * the child, a non-inline function.  (If all the children are inline
   * functions, but a Declaration is captured by a non-inline descendent,
   * then we create a dummy "frame" class.)
   * If (usingCPStyle), then heapFrameLambda is this LambdaExp. (?) */
  LambdaExp heapFrameLambda;

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
  ApplyExp returnContinuation;

  /** If non-null, a Declaration whose value is (only) this LambdaExp. */
  Declaration nameDecl;

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
   * this, if this function is its parent's heapFrame;
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
  static final int NO_FIELD = 256;
  static final int DEFAULT_CAPTURES_ARG = 512;

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

  /** True if this is a method in an ObjectExp. */
  public final boolean isClassMethod()
  { return (flags & CLASS_METHOD) != 0; }

  public final void setClassMethod(boolean isMethod)
  {
    if (isMethod) flags |= CLASS_METHOD;
    else flags &= ~CLASS_METHOD;
  }
  /** The name to give to a dummy implicit function that surrounds a file. */
  public static String fileFunctionName = "atFileLevel";

  /** True iff this is the dummy top-level function of a module body. */
  public final boolean isModuleBody () { return this instanceof ModuleExp; }

  /** True if a class is generated for this procedure.
   * We don't need a class if this is only called inline.
   * We also don't need a class if all callers are known, and we can
   * invoke a method for this procedure.
   * However, the last optimization is not available when using tail calls.
   */
  public final boolean isClassGenerated ()
  {
    return (! getInlineOnly()
	    && (isHandlingTailCalls()
		|| isModuleBody ()
		|| this instanceof ObjectExp
		|| (! outerLambda().isModuleBody()
		    && getCanRead())));
  }

  public final boolean isHandlingTailCalls ()
  {
    return Compilation.usingTailCalls && ! isModuleBody()
      && ! isClassMethod();
  }

  public final boolean variable_args () { return max_args < 0; }

  ClassType type = Compilation.typeProcedure;

  /** Return the ClassType of the Procedure this is being compiled into. */
  public ClassType getCompiledClassType(Compilation comp)
  {
    if (type == Compilation.typeProcedure)
      throw new Error("internal error: getCompileClassType");
    return type;
  }

  public Type getType()
  {
    return type;
  }

  /** Number of argument variable actually passed by the caller.
   * For functions that accept more than 4 argument, or take a variable number,
   * this is 1, since in that all arguments are passed in a single array. */
  public int incomingArgs ()
  {
    // The max_args > 0 is a hack to handle LambdaProecdure, which
    // currently always uses a single array argument.
    return min_args == max_args && max_args <= 4 && max_args > 0 ? max_args : 1;
  }

  /** If non-zero, the selector field of the ModuleMethod for this. */
  int selectorValue;

  int getSelectorValue(Compilation comp)
  {
    if (selectorValue == 0)
      selectorValue = ++comp.maxSelectorValue;
    return selectorValue;
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
    Type[] types = methods[methods.length-1].getParameterTypes();
    return types[types.length-1];
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public String getName () { return name; }

  public String getJavaName ()
  {
    return name == null ? "lambda" : Compilation.mangleName (name);
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
	scope.addVariableAfter(null, thisVariable);
	thisVariable.setParameter (true);  thisVariable.setArtificial (true);
      }
    if (thisVariable.getType() == null)
      thisVariable.setType(clas);
    return thisVariable;
  }

  public Variable declareClosureEnv()
  {
    if (closureEnv == null && getNeedsClosureEnv())
      {
	LambdaExp parent = outerLambda();
	if (parent instanceof ObjectExp)
	  parent = parent.outerLambda();
	Variable parentFrame = parent.heapFrame != null ?  parent.heapFrame
	  : parent.closureEnv;
	if (parent.heapFrameLambda == this || isClassMethod())
	  closureEnv = declareThis(type);
	else if (parent.heapFrame == null && ! parent.getNeedsStaticLink())
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
		scope.addVariableAfter(null, closureEnv);
		closureEnv.setArtificial(true);
		closureEnv.setParameter(true);
	      }
	  }
	else
	  {
	    LambdaExp caller = getInlineOnly() ? getCaller() : null;
	    if (parent == caller)
	      closureEnv = parentFrame;
	    else if (caller != null && parent == caller.outerLambdaNotInline())
	      closureEnv = caller.closureEnv;
	    else
	      {
		closureEnv = new Variable("closureEnv",
                                          parentFrame.getType());
		scope.addVariable(closureEnv);
		closureEnv.setArtificial(true);
	      }
	  }
      }
    return closureEnv;
  }

  public LambdaExp ()
  {
  }

  public LambdaExp (Expression body)
  {
    this.body = body;
  }

  /** Generate code to load heapFrame on the JVM stack. */
  public void loadHeapFrame (Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    LambdaExp curLambda = comp.curLambda;
    while (curLambda != this && curLambda.getInlineOnly())
      curLambda = curLambda.returnContinuation.context;
    if (this == curLambda)
      {
	if (this.heapFrame == null)
	  code.emitPushThis();
	else
	  code.emitLoad(this.heapFrame);
      }
    else
      {
	code.emitLoad(curLambda.closureEnv);
	LambdaExp parent = curLambda.outerLambda();
	while (parent != this)
	  {
	    if (parent.staticLinkField != null)
	      code.emitGetField(parent.staticLinkField);
	    //curLambda = parent;
	    parent = parent.outerLambda();
	  }
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

  public ClassType compile (Compilation comp)
  {
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    Variable saveCallStackContext = comp.callStackContext;
    try
      {
	return comp.addClass (this);
      }
    finally
      {
	comp.curClass = saveClass;
	comp.method = saveMethod;
	comp.callStackContext = saveCallStackContext;
      }
  }

  static Method setNameMethod = null;

  public ClassType compileAlloc (Compilation comp)
  {
    ClassType new_class = compile (comp);
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitNew(new_class);
    code.emitDup(new_class);
    code.emitInvokeSpecial(new_class.constructor);
    if (closureEnvField != null)
      {
	code.emitDup(new_class);
	LambdaExp caller = outerLambda();
	code.emitLoad(caller.heapFrame != null ? caller.heapFrame
		      : caller.closureEnv);		      
	code.emitPutField(closureEnvField);
      }

    if (name != null && ! (this instanceof ObjectExp))
      {
	// Call setName(name) on the result.
	if (setNameMethod == null)
	  setNameMethod
	    = comp.typeProcedure.getDeclaredMethod("setName", 1);
	code.emitDup(new_class);
	code.emitPushString(name);
	code.emitInvokeVirtual(setNameMethod);
      }

    return new_class;
  }

  public void compileEnd (Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (comp.method.reachableHere() && ! isHandlingTailCalls())
      code.emitReturn();
    code.popScope();        // Undoes enterScope in allocParameters
    if (! Compilation.fewerClasses) // FIXME
      code.popScope(); // Undoes pushScope in method.initCode.
  }

  Field allocFieldFor (Compilation comp)
  {
    if (nameDecl != null && nameDecl.field != null)
      return nameDecl.field;
    String name = getName();
    String fname = name == null ? "lambda" : Compilation.mangleName(name);
    int fflags = Access.FINAL;
    if (nameDecl != null && ! nameDecl.isPrivate()
	&& nameDecl.context instanceof ModuleExp)
      fflags |= Access.PUBLIC;
    else
      fname = fname + "$Fn" + ++comp.localFieldIndex;
    Type rtype = Compilation.getMethodProcType(comp.mainClass);
    Field field = comp.mainClass.addField (fname, rtype, fflags);
    if (nameDecl != null)
      nameDecl.field = field;
    return field;
  }

  public Field compileSetField (Compilation comp)
  {
    compileAsMethod(comp);

    comp.applyMethods.addElement(this);

    return (new ProcInitializer(this, comp)).field;
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (getInlineOnly())
      throw new Error("internal error: compile called for inlineOnly LambdaExp");
    LambdaExp parent = outerLambda();
    Type rtype;
    CodeAttr code = comp.getCode();

    /*
    if (Compilation.fewerClasses && ! getImportsLexVars())
      {
	//	Label func_start = new Label(code);
	Label func_end = new Label(code);
	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = this;
	type = saveLambda.type;
	if (comp.usingCPStyle())
	  {
	    heapFrameLambda = this;
	    heapFrame = comp.thisDecl;
	    for (Declaration var = firstDecl();
		 var != null; var = var.nextDecl())
	      var.assignField(comp);
	  }
	gnu.bytecode.SwitchState fswitch = comp.fswitch;
	int pc = comp.fswitch.getMaxValue() + 1;
	code.emitGoto(func_end);
	Type[] stackTypes = code.saveStackTypeState(true);

	fswitch.addCase(pc, code);
	code.emitPushThis();
	code.emitGetField(comp.argsCallStackField);
	code.emitStore(comp.argsArray);
	allocParameters(comp);
	enterFunction(comp);

	body.compileWithPosition(comp, Target.returnObject);
	compileEnd(comp);
	comp.curLambda = saveLambda;
	func_end.define(code);
	code.restoreStackTypeState(stackTypes);
	ClassType ctype = comp.curClass;
	rtype = ctype;
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
		code.emitPutField(comp.nameField);
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
      }
    else
    */
    if (! isClassGenerated())
      {
	rtype = comp.typeModuleMethod;
	if ((flags & NO_FIELD) != 0)
	  {
	    compileAsMethod(comp);
	    ProcInitializer.emitLoadModuleMethod(this, comp);
	  }
	else
	  {
	    Field field = compileSetField(comp);
	    if (field.getStaticFlag())
	      code.emitGetStatic(field);
	    else
	      {
		code.emitPushThis();
		code.emitGetField(field);
	      }
	  }
      }
    else if (parent != null && parent.heapFrameLambda == this)
      {
	// When parent was entered, we allocated an instance of this
	// Procedure, and assigned it to parent's heapFrame.
	// So just get the heapFrame.
	code.emitLoad(parent.heapFrame);
	rtype = parent.heapFrame.getType();
      }
    else
      {
	rtype = compileAlloc (comp);
      }
    target.compileFromStack(comp, rtype);
  }

  void addMethodFor (Compilation comp, ObjectType closureEnvType)
  {
    ClassType ctype = comp.curClass;
    // generate_unique_name (new_class, child.getName());
    String name = getName();
    StringBuffer nameBuf = new StringBuffer(60);
    LambdaExp outer = outerLambda();
    if (comp.generateApplet && outer instanceof ModuleExp)
      closureEnv = declareThis(ctype);
    if (! (outer.isModuleBody() || outer instanceof ObjectExp)
	|| name == null)
      {
	nameBuf.append("lambda");
	nameBuf.append(+(++comp.method_counter));
      }
    if (name != null)
      nameBuf.append(comp.mangleName(name));

    int key_args = keywords == null ? 0 : keywords.length;
    int opt_args = defaultArgs == null ? 0 : defaultArgs.length - key_args;
    int numStubs = ((flags & DEFAULT_CAPTURES_ARG) != 0) ? 0 : opt_args;
    boolean varArgs = max_args < 0 || min_args + numStubs < max_args;
    primMethods = new Method[numStubs + 1];
    int mflags = (isClassMethod() || thisVariable != null) ? Access.PUBLIC
      : closureEnvType == null ? Access.PUBLIC|Access.STATIC
      : closureEnvType == ctype ? Access.PUBLIC|Access.FINAL
      : Access.STATIC;
    Type rtype = body.getType();
    int extraArg = (closureEnvType != null && closureEnvType != ctype) ? 1 : 0;
    name = nameBuf.toString();
    for (int i = 0;  i <= numStubs;  i++)
      {
	int plainArgs = min_args + i;
	int numArgs = plainArgs;
	if (i == numStubs && varArgs)
	  numArgs++;
	Type[] atypes = new Type[extraArg + numArgs];
	if (extraArg > 0)
	  atypes[0] = closureEnvType;
	Declaration var = firstDecl();
	for (int itype = 0; itype < plainArgs; var = var.nextDecl())
	  atypes[extraArg + itype++] = var.getType();
	if (plainArgs < numArgs)
	  {	
	    nameBuf.append("$V");
	    name = nameBuf.toString();
	    Type lastType = var.getType();
	    String lastTypeName = lastType.getName();
	    if (key_args > 0 || numStubs < opt_args
		|| ! ("gnu.kawa.util.LList".equals(lastTypeName)
		      || "java.lang.Object[]".equals(lastTypeName)))
	      {
		lastType = comp.objArrayType;
		argsArray = new Variable("argsArray", comp.objArrayType);
	      }
	    firstArgsArrayArg = var;
	    atypes[atypes.length-1] = lastType;
	  }
	primMethods[i] = ctype.addMethod(name, atypes, rtype, mflags);
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
	if (isHandlingTailCalls())
	  firstArgsArrayArg = decl;
	for (;;)
	  {
	    if (decl == firstArgsArrayArg && argsArray != null)
	      {
		scope.addVariable(argsArray);
		argsArray.setParameter(true);
		argsArray.setArtificial(true);
	      } 
	    if (decl == null)
	      break;
	    Variable var = decl.var;
	    // i is the register to use for the current parameter
	    if (decl.isSimple () && ! decl.isIndirectBinding())
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
                var = decl.var
                  = scope.addVariable(null, decl.getType(), vname);
		//scope.addVariableAfter(var, decl);
		var.setArtificial (true);
		var.setParameter (true);
		//var.allocateLocal(code);
	      }
	    decl = decl.nextDecl();
	  }
      }

    declareClosureEnv();

    for (LambdaExp child = firstChild;  child != null;
	 child = child.nextSibling)
      { 
	if (child.getInlineOnly())
	  continue;
        if (child.isClassGenerated())
	  //            || heapFrameLambda == child
	  //            || child.min_args != child.max_args)
          {
            comp.allocClass(child);
          }
      }

    allocFrame(comp);

    if (getNeedsClosureEnv() && isClassGenerated())
      {
	LambdaExp parent = outerLambda();
	LambdaExp heapFrameLambda = parent.heapFrameLambda;
	if (! (parent instanceof ObjectExp) && heapFrameLambda != this)
	  {
	    if (heapFrameLambda != null)
	      {
		ClassType heapFrameType = heapFrameLambda.getCompiledClassType(comp);
		closureEnvField = comp.curClass.addField ("closureEnv", heapFrameType);
	      }
	    else if (parent.getNeedsStaticLink())
	      {
		closureEnvField = comp.curClass.addField ("closureEnv", parent.closureEnv.getType());
	      }
	  }
      }

    for (LambdaExp child = firstChild;  child != null;
	 child = child.nextSibling)
      {
	if (child.isClassGenerated())
	  {
	    if (child.min_args != child.max_args || child.min_args > 4
		|| child.isHandlingTailCalls())
	      {
		child.argsArray = new Variable("argsArray", comp.objArrayType);
		child.firstArgsArrayArg = child.firstDecl();
	      }
	  }
	else if (! child.getInlineOnly())
	  {
	    boolean method_static;
	    ObjectType closureEnvType;
	    if (! child.getNeedsClosureEnv())
	      closureEnvType = null;
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
	if (heapFrameLambda != null)
	  {
	    frameType = heapFrameLambda.getCompiledClassType(comp);
	  }
	else
	  {
	    frameType = new ClassType(comp.generateClassName("frame"));
	    frameType.setSuper(Type.pointer_type);
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

    code.locals.enterScope (scope);

    if (argsArray != null && isHandlingTailCalls())
      {
        code.emitLoad(comp.callStackContext);
        code.emitGetField(comp.argsCallStackField);
        code.emitStore(argsArray);
      }

    for (Declaration decl = firstDecl();  decl != null;  )
      {
        Variable var = decl.var;
	// If the only reason we are using an argsArray is because there
	// are more than 4 arguments, copy the arguments in local register.
	// Then forget about the args array.  We do this first, before
	// the label that tail-recursion branches back to.
	// This way, a self-tail-call knows where to leave the argumnents.
	if (argsArray != null && min_args == max_args
	    && primMethods == null && ! isHandlingTailCalls())
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

  void enterFunction (Compilation comp)
  {
    CodeAttr code = comp.getCode();

    // Tail-calls loop back to here!
    scope.setStartPC(code.getPC());

    if (closureEnv != null && ! closureEnv.isParameter()
	&& ! getInlineOnly())
      {
	code.emitPushThis();
	Field field = closureEnvField;
	if (field == null)
	  field = outerLambda().closureEnvField;
	code.emitGetField(field);
	code.emitStore(closureEnv);
      }
    if (heapFrame != null && ! comp.usingCPStyle())
      {
	ClassType frameType = (ClassType) heapFrame.getType();
	for (Declaration decl = capturedVars; decl != null;
	     decl = decl.nextCapturedVar)
	  {
	    if (decl.field != null)
	      continue;
     	    String dname = comp.mangleName(decl.getName());
	    String mname = dname;
	    // Check for existing field with same name.
	    for (int i = 0; ; )
	      {
		Field fld = frameType.getField(mname);
		if (fld == null)
		  break;
		mname = dname + '_' + ++i;
	      }
	    Type dtype = decl.getType();
	    decl.field = frameType.addField (mname, decl.getType());
	  }
	if (closureEnv != null && heapFrame != null) 
	  staticLinkField = frameType.addField("staticLink",
					       closureEnv.getType());
        if (! (heapFrameLambda instanceof ModuleExp))
          {
            if (heapFrameLambda != null)
              heapFrameLambda.compileAlloc(comp);
            else
              {
                code.emitNew(frameType);
                code.emitDup(frameType);
                Method constructor = comp.generateConstructor(frameType, null);
                code.emitInvokeSpecial(constructor);
              }
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
    if (min_args == max_args && ! comp.fewerClasses
	&& primMethods == null && ! isHandlingTailCalls())
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

    for (Declaration param = firstDecl();  param != null; param = param.nextDecl())
      {
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
		 : mainMethod.getParameterTypes()[i]);
	    // If the parameter is captured by an inferior lambda,
	    // then the incoming parameter needs to be copied into its
	    // slot in the heapFrame.  Thus we emit an aaload instruction.
	    // Unfortunately, it expects the new value *last*,
	    // so first push the heapFrame array and the array index.
	    if (!param.isSimple ())
	      param.loadOwningObject(comp);
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
		stackType = comp.scmListType;
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
	      CheckedTarget.emitCheckedCoerce(comp, this, i, paramType);
	    if (param.isIndirectBinding())
              param.pushIndirectBinding(comp);
	    if (param.isSimple())
	      code.emitStore(param.getVariable());
            else
	      code.emitPutField(param.field);
	  }
	i++;
      }
  }

  void compileChildMethods (Compilation comp)
  {
    for (LambdaExp child = firstChild;  child != null; )
      {
	if (! child.getCanRead() && ! child.getInlineOnly()
	    && ! child.isHandlingTailCalls())
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
    Type rtype = method.getReturnType();
    Target target = Target.returnValue(rtype);
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

    for (int i = 0;  i <= numStubs;  i++)
      {
	comp.method = primMethods[i];
	if (i < numStubs)
	  {
	    comp.method.init_param_slots();
	    CodeAttr code = comp.getCode();
	    int toCall = i + 1;
	    while (toCall < numStubs
		   && defaultArgs[toCall] instanceof QuoteExp)
	      toCall++;
	    int thisArg = isStatic ? 0 : 1;
	    boolean varArgs = toCall == numStubs && restArgType != null;
	    Declaration decl;
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
	    for (int j = i; j < toCall;  j++, decl = decl.nextDecl())
	      {
		Target paramTarget = StackTarget.getInstance(decl.getType());
		defaultArgs[j].compile(comp, paramTarget);
	      }
	    if (varArgs)
	      {
		Expression arg;
		String lastTypeName = restArgType.getName();
		if ("gnu.kawa.util.LList".equals(lastTypeName))
		  arg = new QuoteExp(gnu.kawa.util.LList.Empty);
		else if ("java.lang.Object[]".equals(lastTypeName))
		  arg = new QuoteExp(Values.noArgs);
		else // FIXME
		  throw new Error("unimplemented #!rest type");
		arg.compile(comp, restArgType);
	      }
	    if (isStatic)
	      code.emitInvokeStatic(primMethods[toCall]);
	    else
	      code.emitInvokeVirtual(primMethods[toCall]);
	    code.emitReturn();
	    closureEnv = null;
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

	    body.compileWithPosition(comp, target);
	    compileEnd(comp);
	  }
      }

    compileChildMethods(comp);
    comp.method = save_method;
    comp.curLambda = save_lambda;
  }

  /** Used to control with .zip files dumps are generated. */
  public static String dumpZipPrefix;
  public static int dumpZipCounter;

  /** A cache if this has already been evaluated. */
  Procedure thisValue;

  public Class evalToClass ()
  {
    try
      {
	String class_name = getJavaName ();

	Compilation comp = new Compilation (this, class_name, null, true);

	byte[][] classes = new byte[comp.numClasses][];
	String[] classNames = new String[comp.numClasses];
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType clas = comp.classes[iClass];
	    classNames[iClass] = clas.getName ();
	    classes[iClass] = clas.writeToArray ();
	  }

	if (dumpZipPrefix != null)
	  {
	    StringBuffer zipname = new StringBuffer(dumpZipPrefix);
	    if (dumpZipCounter >= 0)
	      zipname.append(++dumpZipCounter);
	    zipname.append(".zip");
	    java.io.FileOutputStream zfout
	      = new java.io.FileOutputStream(zipname.toString());
	    java.util.zip.ZipOutputStream zout
	      = new java.util.zip.ZipOutputStream(zfout);
	    for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	      {
		String clname
		  = classNames[iClass].replace ('.', '/') + ".class";
		java.util.zip.ZipEntry zent
		  = new java.util.zip.ZipEntry (clname);
		zent.setSize(classes[iClass].length);
		java.util.zip.CRC32 crc = new java.util.zip.CRC32();
		crc.update(classes[iClass]);
		zent.setCrc(crc.getValue());
		zent.setMethod(java.util.zip.ZipEntry.STORED);
		zout.putNextEntry(zent);
		zout.write(classes[iClass]);
	      }
	    zout.close ();
	  }

	/* DEBUGGING:
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  ClassTypeWriter.print(comp.classes[iClass], System.out, 0);
	*/

	ArrayClassLoader loader = new ArrayClassLoader (classNames, classes);
	Class clas = loader.loadClass (class_name, true);
	/* Pass literal values to the compiled code. */
	for (Initializer init = comp.clinitChain;  init != null;
	     init = init.next)
	  {
	    /* DEBUGGING:
	    OutPort out = OutPort.errDefault();
	    out.print("init["+init.index+"]=");
	    SFormat.print(init.value, out);
	    out.println();
	    */
	    try
	      {
                if (init instanceof Literal)
                  clas.getDeclaredField(init.field.getName())
                    .set(null, ((Literal) init).value);
	      }
	    catch (java.lang.NoSuchFieldException ex)
	      {
		throw new Error("internal error - "+ex);
	      }
	  }
        return clas;
      }
    catch (java.io.IOException ex)
      {
	ex.printStackTrace(OutPort.errDefault());
	throw new RuntimeException ("I/O error in lambda eval: "+ex);
      }
    catch (ClassNotFoundException ex)
      {
	throw new RuntimeException("class not found in lambda eval");
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("class illegal access: in lambda eval");
      }
  }

  public Object eval (Environment env)
  {
    if (thisValue != null)
      return thisValue;
    try
      {
        Class clas = evalToClass();
	Object inst = clas.newInstance ();

	Procedure proc = (Procedure) inst;
	if (proc.getName() == null)
	  proc.setName (this.name);
        thisValue = proc;
	return inst;
      }
    catch (InstantiationException ex)
      {
	throw new RuntimeException("class not instantiable: in lambda eval");
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("class illegal access: in lambda eval");
      }
  }

  Object walk (ExpWalker walker) { return walker.walkLambdaExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%lambda/");
    if (name != null)
      {
	ps.print(name);
	ps.print('/');
      }
    ps.print(id);
    ps.print("/ (");
    Special prevMode = null;
    int i = 0;
    int opt_i = 0;
    int key_args = keywords == null ? 0 : keywords.length;
    int opt_args = defaultArgs == null ? 0 : defaultArgs.length - key_args;
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
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
	if (i > 0)
	  ps.print(' ');
	if (mode != prevMode)
	  {
	    ps.print(mode);
	    ps.print(' ');
	  }
	Expression defaultArg = null;
	if (mode == Special.optional || mode == Special.key)
	  defaultArg = defaultArgs[opt_i++];
	if (defaultArg != null)
	  ps.print('(');
	ps.print(decl.getName());
	if (defaultArg != null && defaultArg != QuoteExp.falseExp)
	  {
	    ps.print(' ');
	    defaultArg.print(ps);
	    ps.print(')');
	  }
	i++;
	prevMode = mode;
      }
    ps.print(") ");
    if (body == null)
      ps.print("<null body>");
    else
      body.print (ps);
    ps.print(")");
  }

  public String toString()
  {
    String str = "LambdaExp/"+name+'/'+id+'/';

	int l = getLine();
	if (l <= 0 && body != null)
	  l = body.getLine();
	if (l > 0)
	  str = str + "l:" + l;

    return str;
  }
}
