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
     The heapFrame is an instance of the Procedure compiled from one
     of our child lambdas.  heapFrameLambda points to the child.  */
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

  static final int INLINE_ONLY = 1;
  static final int CAN_READ = 2;
  static final int CAN_CALL = 4;
  static final int IMPORTS_LEX_VARS = 8;
  int flags;

  /** True iff this lambda is only "called" inline. */
  public final boolean getInlineOnly() { return (flags & INLINE_ONLY) != 0; }
  public final void setInlineOnly(boolean inlineOnly)
  {
    if (inlineOnly) flags |= INLINE_ONLY;
    else flags &= ~INLINE_ONLY;
  }

  public boolean isSharedSwitchMethod() { return false; }

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

    // If this needs an environment (close), the its callers do too.
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
  /** The name to give to a dummy implicit function that surrounds a file. */
  public static String fileFunctionName = "atFileLevel";

  /** True iff this is the dummy top-level function of a module body. */
  public final boolean isModuleBody () { return this instanceof ModuleExp; }

  public final boolean variable_args () { return max_args < 0; }

  ClassType type = Compilation.scmProcedureType;

  /** Return the ClassType of the Procedure this is being compiled into. */
  public ClassType getCompiledClassType()
  {
    if (type == Compilation.scmProcedureType)
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

  public Declaration declareThis(ClassType clas)
  {
    Declaration var  = new Declaration("this", clas);
    scope.addVariableAfter(null, var);
    var.setParameter (true);  var.setArtificial (true);
    return var;
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

  /** If non-null, this is a Field that is used for implementing lexical closures.
   * If getName() is "closureEnv", it is our parent's heapFrame,
   * which is an instance of one of our siblings.
   * (Otherwise, we use "this" as the implicit "closureEnv" field.)
   * If getName() is "staticLink", it is used to chain together heap frames. */
  public Field staticLinkField;

  /** A variable that points to the heap-allocated part of the frame.
   * This is an instance of heapFrameLambda (which is one of our children);
   * each captured variable is a field in the heapFrame. */
  Declaration heapFrame;

  /** Generate code to load heapFrame on the JVM stack. */
  public void loadHeapFrame (Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    LambdaExp curLambda = comp.curLambda;
    while (curLambda != this && curLambda.getInlineOnly())
      curLambda = curLambda.outerLambda();
    if (this == curLambda)
      code.emitLoad(this.heapFrame);
    else
      {
	code.emitPushThis();
	LambdaExp parent = curLambda.outerLambda();
	if (parent.heapFrameLambda != curLambda
	    && parent.heapFrameLambda != null
	    && curLambda.getCanRead())
	  {
	    code.emitGetField(curLambda.staticLinkField);
	  }
	while (parent != this)
	  {
	    if (! (parent instanceof ObjectExp))
	      {
		if (parent.heapFrameLambda != null)
		  curLambda = parent.heapFrameLambda;
		code.emitGetField(curLambda.staticLinkField);
	      }
	    curLambda = parent;
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
    try
      {
	return comp.addClass (this);
      }
    finally
      {
	comp.curClass = saveClass;
	comp.method = saveMethod;
      }
  }

  static Method setNameMethod = null;

  public ClassType compileAlloc (Compilation comp)
  {
    /*
    if (isSharedSwitchMethod())
      {
	code = ...;
	Label l = new Label(code);
	int pc = comp.fwitch.getMaxValue() + 1;
	comp.addCase(pc, l, conp);
	Variable argsArray = declareArgsArray();
	enterFunction(comp, argsArray);
      }
    */
    ClassType new_class = compile (comp);
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitNew(new_class);
    code.emitDup(new_class);
    code.emitInvokeSpecial(new_class.constructor);
    if (staticLinkField != null)
      {
	code.emitDup(new_class);
	LambdaExp caller = outerLambda();
	if (staticLinkField.getName() == "closureEnv")
	  code.emitLoad(caller.heapFrame);
	else // staticLinkField.getName() == "staticLink"
	  {
	    code.emitPushThis();
	    if (caller.staticLinkField != null
		&& caller.staticLinkField.getName() == "closureEnv")
	      code.emitGetField(caller.staticLinkField);
	  }
	code.emitPutField(staticLinkField);
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

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (getInlineOnly())
      throw new Error("internal error: compile called for inlineOnly LambdaExp");
    LambdaExp parent = outerLambda();
    Type rtype;
    if (parent != null && parent.heapFrameLambda == this)
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

  Method addMethodFor (ClassType ctype, String name, int flags)
  {
    Type rtype = body.getType();
    Type[] atypes;
    if (min_args != max_args)
      atypes = Compilation.applyNargs;
    else
      {
	atypes = new Type[max_args];
	Variable var = firstVar ();

	for (int itype = 0;  itype < atypes.length; var = var.nextVar ())
	  {
	    if (! var.isParameter() || var.isArtificial())
	      continue;
	    atypes[itype++] = var.getType();
	  }
      }
    return ctype.addMethod(name, atypes, rtype, flags);
  }

  // Can we merge this with allocParameters?
  public void allocChildClasses (Compilation comp)
  {
    for (LambdaExp child = firstChild;  child != null; ) 
      { 
        if (child.getCanRead() )
	  //            || heapFrameLambda == child 
	  //            || child.min_args != child.max_args) 
          { 
            comp.allocClass(child); 
          } 
        child = child.nextSibling; 
      } 

    if (heapFrame != null)
      {
	ClassType frameType;
	if (heapFrameLambda != null)
	  {
	    frameType = heapFrameLambda.getCompiledClassType();
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

    if (getImportsLexVars())
      {
	LambdaExp parent = outerLambda();
	LambdaExp heapFrameLambda = parent.heapFrameLambda;
	if (parent instanceof ObjectExp)
	  ;
	else if (heapFrameLambda != this && heapFrameLambda != null
	    && getCanRead())
	  {
	    ClassType heapFrameType = heapFrameLambda.getCompiledClassType();
	    staticLinkField = comp.curClass.addField ("closureEnv", heapFrameType);
	  }
	// otherwise: closureEnv==this.
	else if (parent.getImportsLexVars())
	  {
	    Type slinkType = parent.outerLambda().heapFrame.getType();
	    staticLinkField = comp.curClass.addField ("staticLink", slinkType);
	  }
      }

  }

  void allocParameters (Compilation comp, Variable argsArray)
  {
    CodeAttr code = comp.getCode();
    int i = 0;
    Variable var = firstVar();
    while (var != null)
      {
	if (var instanceof Declaration && var.isParameter ())
	  {
	    // i is the register to use for the current parameter
	    Declaration decl = (Declaration) var;
	    if (var.isSimple ())
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
		// It's home location is in the heapFrame.
		// Later, we copy it from it's incoming register
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
		&& ! var.isArtificial())
	      {
		code.emitLoad(argsArray);
		// The first two slots are "this" and "argsArray".
		code.emitPushInt(i-2);
		code.emitArrayLoad(Type.pointer_type);
		code.emitStore(decl);
	      }
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
    if (heapFrame != null)
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
	if (heapFrameLambda != null)
	  {
	    heapFrameLambda.compileAlloc(comp);
	  }
	else
	  {
	    code.emitNew(frameType);
	    code.emitDup(frameType);
	    Method constructor = comp.generateConstructor(frameType, null);
	    code.emitInvokeSpecial(constructor);
	  }
	code.emitStore(heapFrame);
      }

    if (min_args == max_args)
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
	    if (argsArray != null || ! var.isSimple())
	      {
		// If the parameter is captured by an inferior lambda,
		// then the incoming parameter needs to be copied into its
		// slot in the heapFrame.  Thus we emit an aaload instruction.
		// Unfortunately, it expects the new value *last*,
		// so first push the heapFrame array and the array index.
		Declaration param = (Declaration) var;
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
		if (param.isSimple ())
		  code.emitStore(param);
		else
		  code.emitPutField(param.field);
	      }
	    i++;
	  }
      }
  }

  void compile_setLiterals (Compilation comp)
  {
    ClassType[] interfaces = { new ClassType ("gnu.expr.CompiledProc") };
    comp.mainClass.setInterfaces (interfaces);

    Method setLiterals_method
      = comp.mainClass.addMethod ("setLiterals", comp.applyNargs,
				   Type.void_type, Access.PUBLIC);
    setLiterals_method.init_param_slots ();
    CodeAttr code = setLiterals_method.getCode();
    code.emitLoad(code.getArg(1));
    code.emitPutStatic(comp.literalsField);
    code.emitReturn();
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
	compile_setLiterals (comp);

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
	CompiledProc cproc = (CompiledProc) inst;
	Object[] literals = new Object[comp.literalsCount];
	for (Literal literal = comp.literalsChain;  literal != null;
	     literal = literal.next)
	  {
	    /* DEBUGGING:
	    OutPort out = OutPort.errDefault();
	    out.print("literal["+literal.index+"]=");
	    SFormat.print(literal.value, out);
	    out.println();
	    */
	    literals[literal.index] = literal.value;
	  }
	cproc.setLiterals (literals);
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
