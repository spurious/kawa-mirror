// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.util.Hashtable;
import java.io.*;

public class Compilation
{
  public ClassType curClass;
  public ClassType mainClass;

  public LambdaExp curLambda;
  public Declaration thisDecl;

  /** If true, minimize the number of classes generated.
   * Do this even if it makes things a little slower. */
  public static boolean fewerClasses;

  public static boolean usingCPStyle;
  public static boolean usingTailCalls = false;

  ClassType[] classes;
  int numClasses;

  /** True if the compiled result will be immediately loaded. */ 
  boolean immediate;

  /** The current method. */
  public Method method;

  public final CodeAttr getCode() { return method.getCode(); }

  int method_counter;

  /* When multiple procedures are compiled into a single method,
     we use a switch to jump to the correct part of the method. */
  SwitchState fswitch;

  Field fswitchIndex;
  Variable argsArray;
  /** The actual parameter of the current CallFrame.step(CallStack) method. */
  Variable callStackContext;

  // Various standard classes
  static public ClassType scmObjectType = Type.pointer_type;
  static public ClassType scmBooleanType = ClassType.make("java.lang.Boolean");
  static public ClassType javaStringType = ClassType.make("java.lang.String");
  static public ClassType scmSymbolType = javaStringType;
  static public ClassType scmKeywordType = ClassType.make("gnu.expr.Keyword");
  static public ClassType scmSequenceType = ClassType.make("kawa.lang.Sequence");
  static public ClassType javaIntegerType = ClassType.make("java.lang.Integer");
  static public ClassType scmListType = ClassType.make("kawa.lang.List");
  static public ClassType scmPairType = ClassType.make("kawa.lang.Pair");
  static public ClassType scmUndefinedType = ClassType.make("gnu.expr.Undefined");
  static public ClassType scmPatternType = ClassType.make("kawa.lang.Pattern");
  public static final ArrayType objArrayType = new ArrayType (scmObjectType);
  public static final ArrayType symbolArrayType = new ArrayType(scmSymbolType);
  static public ClassType scmNamedType = ClassType.make("gnu.mapping.Named");
  static public ClassType typeProcedure
    = ClassType.make("gnu.mapping.Procedure");
  static public ClassType scmInterpreterType
    = ClassType.make("kawa.lang.Interpreter");
  static public ClassType typeEnvironment
    = ClassType.make("gnu.mapping.Environment");
  static public ClassType typeBinding = ClassType.make("gnu.mapping.Binding");
  static public final Method getBindingMethod
    = typeBinding.addMethod("get", Type.typeArray0,
			    Type.pointer_type, Access.PUBLIC);
  static public final Method getProcedureBindingMethod
    = typeBinding.addMethod("getProcedure", Type.typeArray0,
			    typeProcedure, Access.PUBLIC);
  static public final Field trueConstant
    = scmInterpreterType.addField ("trueObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC); 
  static public final Field falseConstant
    = scmInterpreterType.addField ("falseObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC);
  static final Field voidConstant
  = scmInterpreterType.addField ("voidObject", scmObjectType,
				  Access.PUBLIC|Access.STATIC);
  static final Field undefinedConstant
  = scmInterpreterType.addField ("undefinedObject", scmUndefinedType,
				    Access.PUBLIC|Access.STATIC);
  static final Field nameField
  = scmNamedType.addField ("sym_name", scmSymbolType, Access.PUBLIC);
  static Method initIntegerMethod;
  static Method lookupGlobalMethod;
  static Method defineGlobalMethod;
  static Method putGlobalMethod;
  static Method makeListMethod;
  
  public static final Type[] int1Args = { Type.int_type };
  public static final Type[] string1Arg = { javaStringType };
  public static final Type[] sym1Arg = string1Arg;

  static public final Method getBindingEnvironmentMethod
    = typeEnvironment.addMethod("getBinding", string1Arg,
				typeBinding, Access.PUBLIC);

  static {
    Type[] makeListArgs = { objArrayType, Type.int_type };
    makeListMethod = scmListType.addMethod ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
    initIntegerMethod = javaIntegerType.addMethod ("<init>",
						    int1Args, Type.void_type,
						    Access.PUBLIC);

    lookupGlobalMethod
      = typeEnvironment.addMethod ("lookup_global", sym1Arg,
				   scmObjectType,
				   Access.PUBLIC|Access.STATIC);
    Type[] symObjArgs = { scmSymbolType, scmObjectType };
    defineGlobalMethod
      = typeEnvironment.addMethod ("define_global", symObjArgs,
				   Type.void_type,Access.PUBLIC|Access.STATIC);
    putGlobalMethod
      = typeEnvironment.addMethod ("put_global", symObjArgs,
				   Type.void_type,Access.PUBLIC|Access.STATIC);
  }

  public static Method getCurrentEnvironmentMethod
    = typeEnvironment.addMethod("getCurrent", Type.typeArray0,
				typeEnvironment,Access.PUBLIC|Access.STATIC);

  public static Type[] apply0args = Type.typeArray0;
  public static Type[] apply1args = { scmObjectType };
  public static Type[] apply2args = { scmObjectType, scmObjectType };
  public static Type[] applyNargs = { objArrayType };

  public static final Method makeNullPairMethod
  = scmPairType.addMethod ("makePair", apply0args, scmPairType,
			     Access.PUBLIC|Access.STATIC);
  public static Method makePairMethod;
  static Method checkArgCountMethod;

  public static Method apply0method = typeProcedure.addMethod
  ("apply0", apply0args, scmObjectType, Access.PUBLIC|Access.FINAL);

  public static Method apply1method;
  public static Method apply2method;
  public static Method apply3method;
  public static Method apply4method;
  public static Method applyNmethod;

  static
  {
    apply1method = typeProcedure.addMethod ("apply1", apply1args,
						scmObjectType, Access.PUBLIC);
    apply2method = typeProcedure.addMethod ("apply2", apply2args,
						scmObjectType, Access.PUBLIC);
    Type[] apply3args = { scmObjectType, scmObjectType, scmObjectType };
    apply3method = typeProcedure.addMethod ("apply3", apply3args,
						scmObjectType, Access.PUBLIC);
    Type[] apply4args = { scmObjectType , scmObjectType, scmObjectType, scmObjectType};
    apply4method = typeProcedure.addMethod ("apply4", apply4args,
						scmObjectType, Access.PUBLIC);
    applyNmethod = typeProcedure.addMethod ("applyN", applyNargs,
						scmObjectType, Access.PUBLIC);
    makePairMethod = scmPairType.addMethod ("makePair", apply2args,
					     scmPairType,
					     Access.PUBLIC|Access.STATIC);
    Type[] args = new Type[2];
    args[0] = typeProcedure;
    args[1] = Type.int_type;
    checkArgCountMethod
      = typeProcedure.addMethod("checkArgCount", args, Type.void_type,
				   Access.PUBLIC|Access.STATIC);
  }

  public static Method[] applymethods = {
    apply0method, apply1method, apply2method, apply3method,
    apply4method, applyNmethod };

  public static ClassType typeProcedure0
    = ClassType.make("gnu.mapping.Procedure0", typeProcedure);
  public static ClassType typeProcedure1
    = ClassType.make("gnu.mapping.Procedure1", typeProcedure);
  public static ClassType typeProcedure2
    = ClassType.make("gnu.mapping.Procedure2", typeProcedure);
  public static ClassType typeProcedure3
    = ClassType.make("gnu.mapping.Procedure3", typeProcedure);
  public static ClassType typeProcedure4
    = ClassType.make("gnu.mapping.Procedure4", typeProcedure);
  public static ClassType typeProcedureN
    = ClassType.make("gnu.mapping.ProcedureN", typeProcedure);
  public static ClassType typeModuleBody
    = ClassType.make("gnu.expr.ModuleBody", typeProcedure0);

  /* Classes, fields, and methods used wgen usingCPStyle". */
  public static ClassType typeCallStack
    = ClassType.make("gnu.mapping.CallStack");
  public static Method popCallStackMethod
    = typeCallStack.addMethod("pop", apply0args, Type.void_type,
			      Access.PUBLIC);
  public static Field noArgsProcedureField
    = typeProcedure.addField("noArgs", objArrayType,
				Access.PUBLIC|Access.STATIC);
  public static Field valueCallStackField
    = typeCallStack.addField("value", Type.pointer_type, Access.PUBLIC);
  public static Field pcCallStackField
    = typeCallStack.addField("pc", Type.int_type, Access.PROTECTED);
  public static ClassType typeCpsProcedure
    = ClassType.make("gnu.mapping.CpsProcedure");
  public static ClassType typeCallFrame
    = ClassType.make("gnu.mapping.CallFrame");
  public static Field numArgsCallFrameField
    = typeCallFrame.addField("numArgs", Type.int_type, Access.PROTECTED);
  public static Field argsCallStackField
    = typeCallStack.addField("args", objArrayType, Access.PROTECTED);
  public static Field procCallStackField
    = typeCallStack.addField("proc", typeProcedure, Access.PROTECTED);
  public static Field callerCallFrameField
    = typeCallFrame.addField("caller", typeCallFrame, Access.PROTECTED);
  public static Field saved_pcCallFrameField
    = typeCallFrame.addField("saved_pc", Type.int_type, Access.PROTECTED);
  private static Type[] applyCpsArgs = { typeCallStack};
  public static Method applyCpsMethod
    = typeProcedure.addMethod("apply", applyCpsArgs, Type.void_type,
				 Access.PUBLIC);

  public static ClassType[] typeProcedureArray = {
    typeProcedure0, typeProcedure1, typeProcedure2, typeProcedure3,
    typeProcedure4 };

  Hashtable literalTable;
  int literalsCount;
  Literal literalsChain;

  public static boolean generateMainDefault = false;
  /** True if we should generate a main(String[]) method. */
  public boolean generateMain = generateMainDefault;

  public Literal findLiteral (Object value)
  {
    Literal literal = (Literal) literalTable.get (value);
    if (literal != null)
      {
	// This value is used multiple times (perhaps recursively),
	// so do allocate a LitN Field for it.
	// However, String literals are shared in the constant pool instead.
	if (literal.field == null && ! (literal.value instanceof String))
	  literal.assign (this);
      }
    else
      {
	if (value instanceof Boolean)
	  {
	    boolean val = ((Boolean)value).booleanValue ();
	    literal = new Literal (value,
				   val ? trueConstant : falseConstant,
				   this);
	  }
	else if (value == Values.empty)
	  literal = new Literal (value, voidConstant, this);
	else if (value instanceof Undefined)
	  literal = new Literal (value, undefinedConstant, this);
	else if (immediate)
	  {
	    literal = new Literal (value, this);
	  }
	else if (value instanceof Compilable)
	  literal = ((Compilable) value).makeLiteral (this);
	else if (value instanceof Object[])
	  {
	    Object[] array = (Object[]) value;
	    int len = array.length;
	    literal = new Literal (value, objArrayType, this);
	    for (int i = array.length;  --i >= 0; )
	      {
		Object element = array[i];
		if (element != null)
		  findLiteral(element);
	      }
	  }
	else
	  literal = new Literal (value, scmObjectType, this);
      }
    return literal;
  }

  /** Emit code to push a value.
   * Only used when compiling to a file, and for internal use.
   * Must previously have called findLiteral (to detect cycles).
   */
  public void emitLiteral (Object value)
  {
    Literal literal = (Literal) literalTable.get (value);
    if (literal == null)
      throw new Error ("emitLiteral called without previous findLiteral");
    literal.emit (this, false);
  }

  /** Emit code to "evaluate" a compile-time constant.
   * This is the normal external interface.
   * @param value the value to be compiled
   */
  public void compileConstant (Object value)
  {
    gnu.bytecode.CodeAttr code = getCode();
    if (value == null)
      code.emitPushNull();
    else if (value instanceof String && ! immediate)
      code.emitPushString((String) value);
    else
      {
	Literal literal = findLiteral (value);
	if (literal.field == null)
	  literal.assign (this);
	code.emitGetStatic(literal.field);
      }
  }

  public void compileConstant (Object value, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarg = (ConditionalTarget) target;
	getCode().emitGoto(IfExp.is_true(value) ? ctarg.ifTrue
			   : ctarg.ifFalse);
	return;
      }
    if (target instanceof StackTarget)
      {
	Type type = ((StackTarget) target).getType();
	if (type instanceof PrimType)
	  {
	    try
	      {
		String signature = type.getSignature();
		CodeAttr code = getCode();
		char sig1 = (signature == null || signature.length() != 1) ? ' '
		  : signature.charAt(0);
		if (value instanceof Number)
		  {
		    Number num = (Number) value;
		    switch (sig1)
		      {
		      case 'B':  case 'S':  case 'I':
			code.emitPushInt(num.intValue());
			return;
		      case 'J':
			code.emitPushLong(num.longValue());
			return;
		      case 'F':
			code.emitPushFloat(num.floatValue());
			return;
		      case 'D':
			code.emitPushDouble(num.doubleValue());
			return;
		      }
		  }
		if (sig1 == 'C')
		  {
		    code.emitPushInt((int) ((PrimType) type).charValue(value));
		    return;
		  }
		if (sig1 == 'Z')
		  {
		    boolean val = ((PrimType) type).booleanValue(value);
		    code.emitPushInt(val ? 1 : 0);
		    return;
		  }
	      }
	    catch (ClassCastException ex)
	      {
		// should print an ERROR.
	      }
	  }
      }
    compileConstant(value);
    target.compileFromStack(this, Type.pointer_type);
  }

  private void dumpLiterals ()
  {
    for (Literal literal = literalsChain;  literal != null;
	 literal = literal.next)
      {
	if ((literal.flags & Literal.INITIALIZED) == 0)
	  literal.emit (this, true);
      }
  }

  /** Search this Compilation for a ClassType with a given name.
   * @param name the name of the class desired
   * @return the matching ClassType, or null if none is found */
  public ClassType findNamedClass (String name)
  {
    for (int i = 0;  i < numClasses; i++)
      {
	if (name.equals (classes[i].getName ()))
	  return classes[i];
      }
    return null;
  }

  /** If non-null: a prefix for generateClassName to prepend to names. */
  public String classPrefix;

  /** Convert a string to a safe Java identifier.
   * This is not invertible (since '_' is passed unchanged).
   * This should be fixed. */
  public static String mangleName (String name)
  {
    int len = name.length ();
    StringBuffer mangled = new StringBuffer (len);
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt (i);
	// This function is probably not quite enough ...
	// (Note the verifier may be picky about class names.)
	if (Character.isLowerCase (ch) || Character.isUpperCase (ch)
	    || Character.isDigit (ch) || ch == '_' || ch == '$')
	  mangled.append(ch);
	else if (ch == '!')
	  mangled.append("_B");
	else if (ch == '?')
	  {
	    char first = mangled.length() > 0 ? mangled.charAt(0) : '\0';
	    if (i + 1 == len && Character.isLowerCase(first))
	      {
		mangled.setCharAt(0, Character.toTitleCase(first));
		mangled.insert(0, "is");
	      }
	    else
	      mangled.append("_P");
	  }
	else if (ch == '%')
	  mangled.append("_C");
	else if (ch == '-')
	  {
	    char next = i + 1 < len ? name.charAt(i+1) : '\0';
	    if (next == '>')
	      {
		i++;
		next = i + 1 < len ? name.charAt(i+1) : '\0';
		if (Character.isLowerCase(next))
		  {
		    mangled.append("To");
		    next = Character.toTitleCase(next);
		    mangled.append(next);
		    i++;
		  }
		else
		  mangled.append("_2_");
	      }
	    else if (Character.isLowerCase(next))
	      {
		next = Character.toTitleCase(next);
		mangled.append(next);
		i++;
	      }
	    else
	      mangled.append ("__");
	  }
	else
	  {
	    mangled.append ('_');
	    mangled.append (Character.forDigit ((ch >> 12) & 15, 16));
	    mangled.append (Character.forDigit ((ch >>  8) & 15, 16));
	    mangled.append (Character.forDigit ((ch >>  4) & 15, 16));
	    mangled.append (Character.forDigit ((ch      ) & 15, 16));
	  }
      }
    return mangled.toString ();
  }

  /** Generate an unused class name.
   * @param hint the requested name (or prefix)
   * @return a unique class name.
   */
  public String generateClassName (String hint)
  {
    hint = mangleName (hint);
    if (mainClass != null)
      hint = mainClass.getName() + '$' + hint;
    else if (classPrefix != null)
      hint = classPrefix + hint;
    if (findNamedClass (hint) == null)
      return hint;
    for (int i = 0;  ; i++)
      {
	String new_hint = hint + i;
	if (findNamedClass (new_hint) == null)
	  return new_hint;
      }
  }

  String source_filename;

  public Compilation (LambdaExp lexp, String classname, String prefix,
		      boolean immediate)
  {
    source_filename = lexp.filename;
    classPrefix = prefix;
    this.immediate = immediate;
    
    // Do various code re-writes and optimization.
    ChainLambdas.chainLambdas(lexp);
    PushApply.pushApply(lexp);
    FindTailCalls.findTailCalls(lexp);
    lexp.setCanRead(true);
    if (! usingCPStyle)
      FindCapturedVars.findCapturedVars(lexp);

    mainClass = allocClass (lexp, classname);
    literalTable = new Hashtable (100);
    addClass (lexp);
  }

  public void addClass (ClassType new_class)
  {
    if (source_filename != null)
      new_class.setSourceFile (source_filename);
    if (classes == null)
      classes = new ClassType[20];
    else if (numClasses >= classes.length)
      {
	ClassType[] new_classes = new ClassType[2 * classes.length];
	System.arraycopy (classes, 0, new_classes, 0, numClasses);
	classes = new_classes;
      }
    classes[numClasses++] = new_class;
    new_class.access_flags = Access.PUBLIC;
  }

  ClassType allocClass (LambdaExp lexp)
  {
    String name = lexp.getJavaName();
    name = generateClassName(name);
    return allocClass(lexp, name);
  }

  ClassType allocClass (LambdaExp lexp, String name)
  {
    ClassType type;
    if (lexp instanceof ObjectExp)
      {
	type = lexp.getCompiledClassType(this);
      }
    else
      {
	type = new ClassType(name);
	ClassType superType
	  = lexp.isModuleBody () ? typeModuleBody
	  : usingCPStyle ? typeCallFrame
	  : lexp.isHandlingTailCalls() ? typeCpsProcedure
	  : (lexp.min_args != lexp.max_args || lexp.min_args > 4)
	  ? typeProcedureN
	  : typeProcedureArray[lexp.min_args];
	type.setSuper (superType);
      }

    lexp.type = type;
    addClass(type);
    return type;
  }

  public final Method generateConstructor (ClassType clas, LambdaExp lexp)
  {
    Method save_method = method;
    ClassType save_class = curClass;
    curClass = clas;
    Method constructor_method = clas.addMethod("<init>", Access.PUBLIC,
					       apply0args, Type.void_type);
    clas.constructor = constructor_method;

    Method superConstructor
      = clas.getSuperclass().addMethod("<init>", Access.PUBLIC,
				       apply0args, Type.void_type);
    method = constructor_method;
    constructor_method.init_param_slots ();
    CodeAttr code = getCode();
    code.emitPushThis();
    code.emitInvokeSpecial(superConstructor);

    // If immediate, we cannot set the function name in the constructor,
    // since setLiterals has not been called yet (ecept for nested functions).
    if (lexp != null && lexp.name != null && !immediate)
      {
	constructor_method.compile_push_this ();
	compileConstant (lexp.name);
	code.emitPutField(nameField);
      }
    code.emitReturn();
    method = save_method;
    curClass = save_class;
    return constructor_method;
  }

  /** Compiles a function to a class. */
  public final ClassType addClass (LambdaExp lexp)
  {
    String name;
    ClassType new_class = lexp.type;
    if (new_class == typeProcedure)
      new_class = allocClass(lexp);
    curClass = new_class;
    if (! (fewerClasses && curClass == mainClass))
      lexp.allocChildClasses(this);

    String filename = lexp.getFile();
    lexp.type = new_class;
    if (filename != null)
      new_class.setSourceFile (filename);

    int arg_count;
    char arg_letter;
    LambdaExp saveLambda = curLambda;
    curLambda = lexp;
    Type[] arg_types;
    if (lexp.isHandlingTailCalls())
      {
	arg_count = 1;
	arg_letter = '?';
	arg_types = new Type[1];
	arg_types[0] = typeCallStack;
      }
    else if (lexp.min_args != lexp.max_args || lexp.min_args > 4
	|| (fewerClasses && curClass == mainClass))
      {
	arg_count = 1;
	arg_letter = 'N';
	arg_types = new Type[1];
	arg_types[0] = new ArrayType (scmObjectType);
      }
    else
      {
	arg_count = lexp.min_args;
	arg_letter = Character.forDigit (arg_count, 10);
	arg_types = new Type[arg_count];
	for (int i = arg_count;  --i >= 0; )
	  arg_types[i] = scmObjectType;
      }

    CodeAttr code;
    if (arg_letter == 'N')
      {
	method = curClass.addMethod("numArgs", apply0args, Type.int_type,
				    Access.PUBLIC);
	method.init_param_slots ();
	code = getCode();
	code.emitPushInt(lexp.min_args | (lexp.max_args << 12));
	code.emitReturn();
      }

    Expression body = lexp.body;
    Declaration heapFrame = lexp.heapFrame;

    if (lexp.min_args == lexp.max_args && ! lexp.isModuleBody ()
	&& ! lexp.getImportsLexVars() && ! lexp.getNeedsStaticLink()
	&& ! lexp.isHandlingTailCalls())
      {
	Expression[] args = new Expression[lexp.max_args];

	String // methodJavaName = lexp.getJavaName();
	//if (methodJavaName.equals(curClass.getName()))
	  methodJavaName = "apply";
	Method method = lexp.addMethodFor(curClass, methodJavaName, null);
	this.method = method;
	method.initCode();
	code = getCode();
	lexp.allocParameters(this, null);
	lexp.enterFunction(this, null);
	Type rtype = method.getReturnType();
	body.compileWithPosition(this, Target.returnValue(rtype));
	lexp.compileEnd(this);

	// Set up for compiling regular virtual applyX method.
	// It just calls the static method we just finished with.
	Variable var = lexp.firstVar ();
	lexp.scope = new Scope();
	int itype = 0;
	for ( ;  var != null; var = var.nextVar ())
	  {
	    if (! var.isParameter() || var.isArtificial())
	      continue;
	    String vname = var.getName();
	    Declaration decl = lexp.addDeclaration(vname);
	    decl.setParameter(true);
	    if (var.isArtificial())
	      decl.setArtificial(true);
	    else
	      args[itype++] = new ReferenceExp(vname, decl);
	    if (! var.isSimple())
	      var = var.nextVar();  // Skip xxIncoming fake fields.
	  }
	body = new ApplyExp(new PrimProcedure(method, lexp), args);
	lexp.heapFrame = null;
      }

    Method apply_method;
    if (lexp.isModuleBody())
      apply_method
	  = curClass.addMethod ("run", arg_types, Type.pointer_type,
				Access.PUBLIC|Access.FINAL);
    else if (lexp.isHandlingTailCalls())
      apply_method
	= curClass.addMethod ("apply", arg_types, Type.void_type,
			      Access.PUBLIC|Access.FINAL);
    else
      {
	String apply_name = "apply"+arg_letter;
	apply_method
	  = curClass.addMethod (apply_name, arg_types, scmObjectType,
				Access.PUBLIC|Access.FINAL);
      }
    method = apply_method;

    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    method.initCode();
    code = getCode();

    thisDecl = lexp.declareThis(new_class);
    Variable var = thisDecl;

    if (lexp.isHandlingTailCalls())
      {
	Declaration decl = new Declaration ("stack", typeCallStack);
	Scope scope = lexp.scope;
	scope.addVariableAfter(thisDecl, decl);
	decl.setParameter(true);
	decl.setArtificial(true);
	callStackContext = decl;
      }

    Variable argsArray;
    if (lexp.isHandlingTailCalls() || lexp.min_args != lexp.max_args || lexp.min_args > 4
	|| (fewerClasses && curClass == mainClass))
      argsArray = lexp.declareArgsArray();
    else
      argsArray = null;

    if (usingCPStyle() || (fewerClasses && curClass == mainClass))
      {
	code = getCode();
	if (lexp.isHandlingTailCalls())
	  {
	    fswitchIndex = pcCallStackField;
	    callStackContext.reserveLocal(1, code);
	    code.emitLoad(callStackContext);
	  }
	else
	  {
	    fswitchIndex = curClass.addField("PC", Type.int_type);
	    code.emitPushThis();
	  }
	code.emitGetField(fswitchIndex);
	fswitch = new SwitchState(code);
	fswitch.addCase(0, code);
	this.argsArray = argsArray;
      }

    int line = lexp.getLine();
    if (line > 0)
      code.putLineNumber(line);

    if (arg_letter == 'N')
      {
	argsArray.reserveLocal(1, code);

	if (true) // If generating code to check number of arguments
	  {
	    code.emitPushThis();
	    code.emitLoad(argsArray);
	    code.emitArrayLength();
	    code.emitInvokeStatic(checkArgCountMethod);
	  }
      }

    lexp.allocParameters(this, argsArray);
    lexp.enterFunction(this, argsArray);

    body.compileWithPosition(this,
			     lexp.isModuleBody () ? Target.pushObject
			     : Target.returnObject);
    lexp.compileEnd(this);
    if (Compilation.fewerClasses) // FIXME
      method.popScope(); // Undoes pushScope in method.initCode.

    lexp.heapFrame = heapFrame;  // Restore heapFrame.
    lexp.compileChildMethods(this);
    if (usingCPStyle() || (fewerClasses && curClass == mainClass))
      {
	code = getCode();
	fswitch.finish(code);
      }

    if (curClass == mainClass
	&& ((! immediate && literalsChain != null)
	    || bindingFields.size() > 0))
      {
	Method save_method = method;
	method = curClass.addMethod ("<clinit>", apply0args, Type.void_type,
				     Access.PUBLIC|Access.STATIC);
	method.init_param_slots ();
	if (! immediate)
	  dumpLiterals ();
	code = getCode();
	int numGlobals = bindingFields.size();
	if (numGlobals > 0)
	  {
	    code.emitInvokeStatic(getCurrentEnvironmentMethod);
	    java.util.Enumeration e = bindingFields.keys();
	    while (e.hasMoreElements())
	      {
		String id = (String) e.nextElement();
		Field fld = (Field) bindingFields.get(id);
		if (--numGlobals > 0)
		  code.emitDup(1);
		code.emitPushString(id);
		code.emitInvokeVirtual(getBindingEnvironmentMethod);
		code.emitPutStatic(fld);
	      }
	  }
	code.emitReturn();
	method = save_method;
      }

    generateConstructor (curClass, lexp);

    curLambda = saveLambda;

    if (generateMain && lexp.isModuleBody () && curClass == mainClass)
      {
	Type[] args = { new ArrayType(javaStringType) };
	method = curClass.addMethod("main", Access.PUBLIC|Access.STATIC,
				    args, Type.void_type);
				    
	method.init_param_slots ();
	code = getCode();
	code.emitNew(curClass);
	code.emitDup(curClass);
	code.emitInvokeSpecial(curClass.constructor);
	code.emitLoad(code.getArg(0));
	Method moduleMain
	  = typeModuleBody.addMethod("runAsMain", Access.PUBLIC,
				     args, Type.void_type);
	code.emitInvokeVirtual(moduleMain);
	code.emitReturn();
      }

    return new_class;
  }

  public static boolean usingCPStyle() { return usingCPStyle; }
  public static boolean usingTailCalls() { return usingTailCalls; }

  int localFieldIndex; 
  public Field allocLocalField (Type type, String name)
  {
    if (name == null)
      name = "tmp_"+(++localFieldIndex);
    Field field = curClass.addField(name, type, 0);
    return field;
  }

  public void freeLocalField (Field field)
  {
    // FIXME
  }

  Hashtable bindingFields = new Hashtable(100);

  /** Allocate a static Binding field used to access globals bindings. */
  public Field getBindingField (String name)
  {
    Object fld = bindingFields.get(name);
    if (fld != null)
      return (Field) fld;
    String fieldName = "id"+bindingFields.size()+"$"+mangleName(name);
    Field field = mainClass.addField(fieldName, typeBinding, Access.STATIC);
    bindingFields.put(name, field);
    return field;
  }

  String filename;
  int position;

  public void error(char severity, String message)
  {
    error(severity, filename, position >> 12, position & ((1 << 12) - 1), message);
  }

  public void error(char severity, String filename, int line, int column, String message)
  {
    error(new gnu.text.SourceError(severity, filename, line, column, message));
  }

  public void error(gnu.text.SourceError err)
  {
    // FIXME - use SourceMessages framework!
    System.err.println(err);
  }
}
