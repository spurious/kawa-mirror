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
  public Keyword[] keywords;
  public Expression[] defaultArgs;

  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  int id = ++counter;

  /** A list of Declarations, chained using Declaration's nextCapturedVar.
    * All the Declarations are allocated in the current heapFrame. */
  Declaration capturedVars;
  /* If any variables local to this LambdaExp are captured by some inner
     non-lined Lambda, then all such variables are allocated in a heapFrame.
     If (! usingCPStyle), then the heapFrame is an instance of the Procedure
     compiled from one of our child lambdas, and
     heapFrameLambda points to the child
     If (! usingCPStyle), then heapFrameLambda is this LambdaExp.  */
  LambdaExp heapFrameLambda;

  public LambdaExp firstChild;
  public LambdaExp nextSibling;

  /** A magic value to indicate there is no unique return continuation. */
  final static ApplyExp unknownContinuation = new ApplyExp (null, null);

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

  /** Field in heapFrame,getType() that contains the static link.
   * Its value is this function's heapFrame value. */
  public Field staticLinkField;

  /** A variable that points to the heap-allocated part of the frame.
   * This is an instance of heapFrameLambda (which is one of our children);
   * each captured variable is a field in the heapFrame. */
  Declaration heapFrame;

  /** A variable that points to the closure environment passed in.
   * It can be any one of:
   * null, if no closure environment is needed;
   * this, if this function is its parent's heapFrame;
   * a local variable initialized from this.closureEnv;
   * a parameter (only if !getCanRead()); or
   * a copy of our parent's closureEnv or heapFrame (only if getInlineOnly()).
   * See declareClosureEnv and closureEnvField. */
  Declaration closureEnv;

  static final int INLINE_ONLY = 1;
  static final int CAN_READ = 2;
  static final int CAN_CALL = 4;
  static final int IMPORTS_LEX_VARS = 8;
  static final int NEEDS_STATIC_LINK = 16;
  /* Used (future) by FindTailCalls. */
  static final int CANNOT_INLINE = 32;
  static final int CLASS_METHOD = 64;
  int flags;

  /** True iff this lambda is only "called" inline. */
  public final boolean getInlineOnly() { return (flags & INLINE_ONLY) != 0; }
  public final void setInlineOnly(boolean inlineOnly)
  {
    if (inlineOnly) flags |= INLINE_ONLY;
    else flags &= ~INLINE_ONLY;
  }

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
      {
	LambdaExp outer = outerLambda();
	for (ApplyExp app = nameDecl.firstCall;
	     app != null;  app = app.nextCall)
	  {
	    LambdaExp caller = app.context;
	    for (; caller != outer; caller = caller.outerLambda())
	      caller.setImportsLexVars();
	  }
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
   * However, the last optimization is not availble when using tail calls.
   */
  public final boolean isClassGenerated ()
  {
    return (! getInlineOnly()
	    && (getCanRead() || isHandlingTailCalls()));
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

  /** Method used to implement this function. */
  Method primMethod;

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

  Declaration thisVariable;

  public Declaration declareThis(ClassType clas)
  {
    if (thisVariable == null)
      {
	Declaration var  = new Declaration("this", clas);
	scope.addVariableAfter(null, var);
	var.setParameter (true);  var.setArtificial (true);
	thisVariable = var;
      }
    return thisVariable;;
  }

  public Declaration declareClosureEnv()
  {
    if (closureEnv == null && (getImportsLexVars() || getNeedsStaticLink()))
      {
	LambdaExp parent = outerLambda();
	if (parent instanceof ObjectExp)
	  parent = parent.outerLambda();
	Declaration parentFrame = parent.heapFrame != null ?  parent.heapFrame
	  : parent.closureEnv;
	if (getInlineOnly())
	  {
	    closureEnv = parentFrame;
	  }
	else if (parent.heapFrameLambda == this)
	  closureEnv = thisVariable;
	else if (! isClassGenerated())
	  {
	    if (parent.heapFrame == null)
	      closureEnv = null;
	    else if (! primMethod.getStaticFlag())
	      closureEnv = declareThis(primMethod.getDeclaringClass());
	    else
	      {
		Type envType = primMethod.getParameterTypes()[0];
		closureEnv = new Declaration("closureEnv", envType);
		scope.addVariableAfter(null, closureEnv);
		closureEnv.setArtificial(true);
		closureEnv.setParameter(true);
	      }
	  }
	else if (parent.heapFrame == null && ! parent.getNeedsStaticLink())
	  closureEnv = null;
	else
	  {
	    closureEnv = new Declaration("closureEnv",
					 parentFrame.getType());
	    scope.addVariable(closureEnv);
	    closureEnv.setArtificial(true);
	  }
      }
    return closureEnv;
  }

  public Declaration declareArgsArray()
  {
    Variable prev = scope.firstVar();
    if (prev != null)
      {
	if (prev.getName() == "argsArray")
	  return (Declaration) prev;
	if (prev.getName() == "this")
	  {
	    Variable next = prev.nextVar();
	    if (next != null && next.getName() == "argsArray")
	      return (Declaration) next;
	  }
	else
	  prev = null;
      }
    Declaration decl = new Declaration ("argsArray", Compilation.objArrayType);
    // The "argsArray" is the second variable allocated (after "this").
    if (isHandlingTailCalls())
      prev = prev.nextVar();
    scope.addVariableAfter(prev, decl);
    decl.setParameter(true);
    decl.setArtificial(true);
    return decl;
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
    for (Variable var = firstVar ();  ; var = var.nextVar ())
      {
	if (var == null)
	  throw new Error ("internal error - getArg");
	if (var.isParameter () && !var.isArtificial ())
	  {
	    if (i == 0)
	      return (Declaration) var;
	    --i;
	  }
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

    if (name != null)
      {
	// Call setName(name) on the result.
	if (setNameMethod == null)
	  {
	    ClassType typeNamed = ClassType.make("gnu.mapping.Named");
	    setNameMethod
	      = typeNamed.addMethod("setName",
				    Compilation.string1Arg, Type.void_type,
				    Access.PUBLIC|Access.FINAL);
	  }
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
    code.popScope();        // Undoes enterScope in enterFuntion.
    if (! Compilation.fewerClasses) // FIXME
      comp.method.popScope(); // Undoes pushScope in method.initCode.
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (getInlineOnly())
      throw new Error("internal error: compile called for inlineOnly LambdaExp");
    LambdaExp parent = outerLambda();
    Type rtype;
    if (Compilation.fewerClasses && ! getImportsLexVars())
      {
	CodeAttr code = comp.getCode();
	//	Label func_start = new Label(code);
	Label func_end = new Label(code);
	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = this;
	type = saveLambda.type;
	if (comp.usingCPStyle())
	  {
	    heapFrameLambda = this;
	    heapFrame = comp.thisDecl;
	    for (Variable var = firstVar (); var != null; var = var.nextVar ())
	      {
		if (! var.isArtificial())
		  ((Declaration) var).assignField(comp);
	      }
	  }
	gnu.bytecode.SwitchState fswitch = comp.fswitch;
	int pc = comp.fswitch.getMaxValue() + 1;
	code.emitGoto(func_end);
	Type[] stackTypes = code.saveStackTypeState(true);

	fswitch.addCase(pc, code);
	//	func_start.define(code);
	//comp.method.initCode();
	code.emitPushThis();
	code.emitGetField(comp.argsCallStackField);
	code.emitStore(comp.argsArray);
	allocParameters(comp, comp.argsArray);
	enterFunction(comp, comp.argsArray);

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
    else if (parent != null && parent.heapFrameLambda == this)
      {
	// When parent was entered, we allocated an instance of this
	// Procedure, and assigned it to parent's heapFrame.
	// So just get the heapFrame.
	parent.heapFrame.load(comp);
	rtype = parent.heapFrame.getType();
      }
    else
      {
	rtype = compileAlloc (comp);
      }
    target.compileFromStack(comp, rtype);
  }

  Method addMethodFor (ClassType ctype, String name, ObjectType closureEnvType)
  {
    Type rtype = body.getType();
    Type[] atypes;
    int extraArg = (closureEnvType != null && closureEnvType != ctype) ? 1 : 0;
    if (min_args != max_args)
      {
	if (extraArg == 0)
	  atypes = Compilation.applyNargs;
	else
	  {
	    atypes = new Type[2];
	    atypes[0] = closureEnvType;
	    atypes[1] = Compilation.objArrayType;
	  }
      }
    else
      {
	int itype = 0;
	atypes = new Type[extraArg + max_args];
	if (extraArg > 0)
	  atypes[0] = closureEnvType;
	Variable var = firstVar ();

	for ( ; itype < max_args; var = var.nextVar ())
	  {
	    if (! var.isParameter() || var.isArtificial())
	      continue;
	    atypes[extraArg + itype++] = var.getType();
	  }
      }
    int flags = closureEnvType == null ? Access.PUBLIC|Access.STATIC
      : closureEnvType == ctype ? Access.FINAL
      : Access.STATIC;
    return ctype.addMethod(name, atypes, rtype, flags);
  }

  // Can we merge this with allocParameters?
  public void allocChildClasses (Compilation comp)
  {
    for (LambdaExp child = firstChild;  child != null; ) 
      { 
        if (child.isClassGenerated())
	  //            || heapFrameLambda == child 
	  //            || child.min_args != child.max_args) 
          { 
            comp.allocClass(child); 
          } 
        child = child.nextSibling; 
      }
    allocFrame(comp);
  }

  public void allocFrame (Compilation comp)
  {
    if (heapFrame != null)
      {
	ClassType frameType;
	if (heapFrameLambda != null)
	  {
	    frameType = heapFrameLambda.getCompiledClassType(comp);
	    //frameType = comp.curClass;
	  }
	else
	  {
	    frameType = new ClassType(comp.generateClassName("frame"));
	    frameType.setSuper(Type.pointer_type);
	    comp.addClass(frameType);
	  }
	heapFrame.setType(frameType);
      }

    if ((getImportsLexVars() || getNeedsStaticLink())
	&& isClassGenerated())
      {
	LambdaExp parent = outerLambdaNotInline();
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

  }

  void allocParameters (Compilation comp, Variable argsArray)
  {
    CodeAttr code = comp.getCode();
    int i = 0;
    int j = 0;
    declareClosureEnv();
    Variable var = firstVar();
    while (var != null)
      {
	if (var instanceof Declaration && var.isParameter ())
	  {
	    // i is the register to use for the current parameter
	    Declaration decl = (Declaration) var;
	    if (var.isSimple () && ! decl.isIndirectBinding())
	      {
		// For a simple parameter not captured by an inferior lambda,
		// just allocate it in the incoming register.  This case also
		// handles the artificial "this" and "argsArray" variables.
		var.allocateLocal(code);
	      }
	    else if (min_args != max_args)
	      {
		// The incoming value is an element in the argsArray variable
		// (or many elements in the case of a "rest" parameter).
		// We do not need to do anything here (but see below).
	      }
	    else
	      {
		// This variable was captured by an inner lambda.
		// Its home location is in the heapFrame.
		// Later, we copy it from its incoming register
		// to its home location heapFrame.  Here we just create and
		// assign a Variable for the incoming (register) value.
		String incoming_name = (var.getName ()+"Incoming").intern();
		decl = new Declaration(incoming_name);
		scope.addVariableAfter(var, decl);
		decl.setArtificial (true);
		decl.setParameter (true);
		decl.allocateLocal(code);
	      }
	    
	    // If the only reason we are using an argsArray is because there
	    // are more than 4 arguments, copy the arguments in local register.
	    // Then forget about the args array.  We do this first, before
	    // the label that tail-recursion branches back to.
	    // This way, a self-tail-call knows where to leave the argumnents.
	    if (argsArray != null && min_args == max_args
		// && ! comp.fewerClasses
		&& ! var.isArtificial())
	      {
		code.emitLoad(argsArray);
		code.emitPushInt(j);
		code.emitArrayLoad(Type.pointer_type);
		decl.getType().emitCoerceFromObject(code);
		code.emitStore(decl);
	      }
	    if (var == argsArray && isHandlingTailCalls())
	      {
		code.emitLoad(comp.callStackContext);
		code.emitGetField(comp.argsCallStackField);
		code.emitStore(argsArray);
	      }
	    if (! var.isArtificial())
	      j++;
	    if (var != decl)
	      var = decl;  // Skip "incomingXxx" before next iteration.
	    i++;
	  }
	var = var.nextVar();
      }
  }

  static Method searchForKeywordMethod;

  void enterFunction (Compilation comp, Variable argsArray)
  {
    CodeAttr code = comp.getCode();
    // Tail-calls loop back to here!
    code.enterScope (scope);
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
	    String dname = comp.mangleName(decl.getName());
	    String mname = dname;
	    // Check for existing field with name name.  Probably overkill.
	    for (int i = 0; ; i++)
	      {
		Field fld = frameType.getField(mname);
		if (fld == null)
		  break;
		mname = dname + 1;
	      }
	    Type dtype = decl.getType();
	    decl.field = frameType.addField (mname, decl.getType());
	  }
	if (closureEnv != null && heapFrame != null) 
	  staticLinkField = frameType.addField("staticLink",
					       closureEnv.getType());
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

    if (min_args == max_args && ! comp.fewerClasses)
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
    for (Variable var = firstVar ();  var != null; var = var.nextVar ())
      {
	if (var.isParameter () && ! var.isArtificial ())
	  {
	    Declaration param = (Declaration) var;
	    if (argsArray != null || ! var.isSimple()
		|| param.isIndirectBinding())
	      {
		// If the parameter is captured by an inferior lambda,
		// then the incoming parameter needs to be copied into its
		// slot in the heapFrame.  Thus we emit an aaload instruction.
		// Unfortunately, it expects the new value *last*,
		// so first push the heapFrame array and the array index.
		if (!param.isSimple ())
		  {
		    param.loadOwningObject(comp);
		  }
		// This part of the code pushes the incoming argument.
		if (argsArray == null)
		  {
		    // Simple case:  Use Incoming register.
		    code.emitLoad(param.nextVar());
		  }
		else if (i < min_args)
		  { // This is a required parameter, in argsArray[i].
		    code.emitLoad(argsArray);
		    code.emitPushInt(i);
		    code.emitArrayLoad(Type.pointer_type);
		  }
		else if (i < min_args + opt_args)
		  { // An optional parameter
		    code.emitPushInt(i);
		    code.emitLoad(argsArray);
		    code.emitArrayLength();
		    code.emitIfIntLt();
		    code.emitLoad(argsArray);
		    code.emitPushInt(i);
                    code.emitArrayLoad(Type.pointer_type);
		    code.emitElse();
		    defaultArgs[opt_i++].compile(comp, Target.pushObject);
		    code.emitFi();
		  }
		else if (max_args < 0 && i == min_args + opt_args)
		  {
		    // This is the "rest" parameter (i.e. following a "."):
		    // Convert argsArray[i .. ] to a list.
		    code.emitLoad(argsArray);
		    code.emitPushInt(i);
		    code.emitInvokeStatic(Compilation.makeListMethod);
		  }
		else
		  { // Keyword argument.
		    if (searchForKeywordMethod == null)
		      {
			Type[] argts = new Type[3];
			argts[0] = Compilation.objArrayType;
			argts[1] = Type.int_type;
			argts[2] = Type.pointer_type;
			searchForKeywordMethod
			  = Compilation.scmKeywordType.addMethod("searchForKeyword",
						      argts, Type.pointer_type,
						      Access.PUBLIC|Access.STATIC);
		      }
		    code.emitLoad(argsArray);
		    code.emitPushInt(min_args + opt_args);
		    comp.compileConstant(keywords[key_i++]);
		    code.emitInvokeStatic(searchForKeywordMethod);
		    code.emitDup(1);
		    comp.compileConstant(Special.dfault);
		    code.emitIfEq();
		    code.emitPop(1);
		    defaultArgs[opt_i++].compile(comp, Target.pushObject);
		    code.emitFi();
		  }
		// Now finish copying the incoming argument into its
		// home location.
		param.getType().emitCoerceFromObject(code);
		if (param.isIndirectBinding())
		  param.pushIndirectBinding(comp);
		if (param.isSimple())
		  code.emitStore(param);
		else
		  code.emitPutField(param.field);
	      }
	    i++;
	  }
      }
  }

  /** Used to control with .zip files dumps are generated. */
  public static String dumpZipPrefix;
  public static int dumpZipCounter;

  public Object eval (Environment env)
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
	Object inst = clas.newInstance ();

	/* Pass literal values to the compiled code. */
	for (Literal literal = comp.literalsChain;  literal != null;
	     literal = literal.next)
	  {
	    /* DEBUGGING:
	    OutPort out = OutPort.errDefault();
	    out.print("literal["+literal.index+"]=");
	    SFormat.print(literal.value, out);
	    out.println();
	    */
	    try
	      {
		clas.getDeclaredField(literal.field.getName())
		  .set(null, literal.value);
	      }
	    catch (java.lang.NoSuchFieldException ex)
	      {
		throw new Error("internal error - "+ex);
	      }
	  }
	Named named = (Named) inst;
	if (named.name () == null)
	  named.setName (this.name);

	return inst;
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
    for (Variable var = firstVar ();  var != null; var = var.nextVar ())
      {
	if (! var.isParameter () || var.isArtificial ())
	  continue;
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
	ps.print(((Declaration)var).string_name());
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
