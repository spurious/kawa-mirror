package kawa.lang;
import gnu.bytecode.*;
import java.util.Hashtable;
import java.io.*;

public class Compilation
{
  public ClassType curClass;
  public ClassType mainClass;

  public LambdaExp curLambda;

  ClassType[] classes;
  int numClasses;

  /** True if the compiled result will be immediately loaded. */ 
  boolean immediate;

  /** The current method. */
  public Method method;

  public final CodeAttr getCode() { return method.getCode(); }

  // Various standard classes
  static public ClassType scmObjectType = new ClassType ("java.lang.Object");
  static public ClassType scmBooleanType = new ClassType ("java.lang.Boolean");
  static public ClassType javaStringType = new ClassType ("java.lang.String");
  static public ClassType scmSymbolType = javaStringType;
  static public ClassType scmKeywordType = new ClassType ("kawa.lang.Keyword");
  static public ClassType scmSequenceType = new ClassType ("kawa.lang.Sequence");
  static public ClassType javaIntegerType = new ClassType ("java.lang.Integer");
  static public ClassType scmListType = new ClassType ("kawa.lang.List");
  static public ClassType scmPairType = new ClassType ("kawa.lang.Pair");
  static public ClassType scmUndefinedType = new ClassType ("kawa.lang.Undefined");
  static public ClassType scmPatternType = new ClassType ("kawa.lang.Pattern");
  static ArrayType objArrayType = new ArrayType (scmObjectType);
  static ArrayType symbolArrayType = new ArrayType (scmSymbolType);
  static public ClassType scmNamedType = new ClassType ("kawa.lang.Named");
  static public ClassType scmProcedureType
    = new ClassType ("kawa.lang.Procedure");
  static public ClassType scmInterpreterType
    = new ClassType ("kawa.lang.Interpreter");
  static public ClassType scmEnvironmentType
    = new ClassType ("kawa.lang.Environment");
  static final Field carField
    = scmPairType.addField ("car", scmObjectType, Access.PUBLIC);
  static final Field cdrField
    = scmPairType.addField ("cdr", scmObjectType, Access.PUBLIC);
  static final Field trueConstant
    = scmInterpreterType.addField ("trueObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC); 
  static final Field falseConstant
    = scmInterpreterType.addField ("falseObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC);
  static final Field nullConstant
  = scmListType.addField ("Empty", scmListType, Access.PUBLIC|Access.STATIC);
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
  static Method makeListMethod;
  static Method searchForKeywordMethod;
  
  static Type[] int1Args = { Type.int_type };
  static Type[] string1Arg = { javaStringType };
  public static Type[] sym1Arg = string1Arg;

  static {
    Type[] makeListArgs = { objArrayType, Type.int_type };
    makeListMethod = scmListType.addMethod ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
    initIntegerMethod = javaIntegerType.addMethod ("<init>",
						    int1Args, Type.void_type,
						    Access.PUBLIC);

    lookupGlobalMethod
      = scmEnvironmentType.addMethod ("lookup_global", sym1Arg,
				       scmObjectType,
				       Access.PUBLIC|Access.STATIC);
    Type[] symObjArgs = { scmSymbolType, scmObjectType };
    defineGlobalMethod
      = scmEnvironmentType.addMethod ("define_global", symObjArgs,
				       Type.void_type,
				       Access.PUBLIC|Access.STATIC);
  }

  public static Type[] apply0args = Type.typeArray0;
  public static Type[] apply1args = { scmObjectType };
  public static Type[] apply2args = { scmObjectType, scmObjectType };
  public static Type[] applyNargs = { objArrayType };

  static final Method makeNullPairMethod
  = scmPairType.addMethod ("makePair", apply0args, scmPairType,
			     Access.PUBLIC|Access.STATIC);
  static Method makePairMethod;

  static Method checkArgCountMethod;

  public static Method apply0method = scmProcedureType.addMethod
  ("apply0", apply0args, scmObjectType, Access.PUBLIC|Access.FINAL);

  public static Method apply1method;
  public static Method apply2method;
  public static Method apply3method;
  public static Method apply4method;
  public static Method applyNmethod;

  static
  {
    apply1method = scmProcedureType.addMethod ("apply1", apply1args,
						scmObjectType, Access.PUBLIC);
    apply2method = scmProcedureType.addMethod ("apply2", apply2args,
						scmObjectType, Access.PUBLIC);
    Type[] apply3args = { scmObjectType, scmObjectType, scmObjectType };
    apply3method = scmProcedureType.addMethod ("apply3", apply3args,
						scmObjectType, Access.PUBLIC);
    Type[] apply4args = { scmObjectType , scmObjectType, scmObjectType, scmObjectType};
    apply4method = scmProcedureType.addMethod ("apply4", apply4args,
						scmObjectType, Access.PUBLIC);
    applyNmethod = scmProcedureType.addMethod ("applyN", applyNargs,
						scmObjectType, Access.PUBLIC);
    makePairMethod = scmPairType.addMethod ("makePair", apply2args,
					     scmPairType,
					     Access.PUBLIC|Access.STATIC);
    Type[] args = new Type[2];
    args[0] = scmProcedureType;
    args[1] = Type.int_type;
    checkArgCountMethod
      = scmProcedureType.addMethod("checkArgCount", args, Type.void_type,
				   Access.PUBLIC|Access.STATIC);
  }

  public static Method[] applymethods = {
    apply0method, apply1method, apply2method, apply3method,
    apply4method, applyNmethod };

  Hashtable literalTable;
  int literalsCount;
  Literal literalsChain;
  /* The static "literals" field, which points to an array of literal values.
   * Only used if immdiate. */
  Field literalsField;

  public static boolean generateMainDefault = false;
  /** True if we should generate a main(String[]) method. */
  public boolean generateMain = generateMainDefault;

  Literal findLiteral (Object value)
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
	else if (value == Interpreter.voidObject)
	  literal = new Literal (value, voidConstant, this);
	else if (value == List.Empty)
	  literal = new Literal (value, nullConstant, this);
	else if (value == Interpreter.undefinedObject)
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
	      findLiteral (array[i]);
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
  void emitLiteral (Object value)
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
	if (literal.field == literalsField)
	  {
	    code.emitPushInt(literal.index);
	    code.emitArrayLoad(scmObjectType);
	    method.maybe_compile_checkcast (literal.type);
	  }
      }
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
	    || Character.isDigit (ch) || ch == '_')
	  mangled.append(ch);
	else if (ch == '!')
	  mangled.append("_B");
	else if (ch == '?')
	  mangled.append("_P");
	else if (ch == '%')
	  mangled.append("_C");
	else if (ch == '-')
	  {
	    if (i + 1 < len && name.charAt(i+1) == '>')
	      {
		mangled.append("_2_");
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

  public Compilation (ModuleExp mexp, String classname, String prefix)
  {
    source_filename = mexp.filename;
    classPrefix = prefix;
    addClass (mexp, classname);
  }

  public Compilation (LambdaExp lexp, String classname, boolean immediate)
  {
    source_filename = lexp.filename;
    this.immediate = immediate;
    addClass (lexp, classname);
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

  /** Compiles a function to a class. */
  public final ClassType addClass (LambdaExp lexp, String name)
  {
    ClassType new_class = new ClassType (name);
    curClass = new_class;
    if (mainClass == null)
      {
	mainClass = curClass;
	literalTable = new Hashtable (100);
	if (immediate)
	  literalsField = new_class.addField ("literals",
					       objArrayType, Access.STATIC);
      }
    addClass (new_class);

    int arg_count;
    char arg_letter;
    LambdaExp saveLambda = curLambda;
    curLambda = lexp;
    Type[] arg_types;
    Variable argsArray;
    if (lexp.min_args != lexp.max_args || lexp.min_args > 4)
      {
	arg_count = 1;
	arg_letter = 'N';
	arg_types = new Type[1];
	arg_types[0] = new ArrayType (scmObjectType);

	// The "argsArray" is the second variable allocated (after "this").
	argsArray = lexp.firstVar().nextVar();
      }
    else
      {
	arg_count = lexp.min_args;
	arg_letter = Character.forDigit (arg_count, 10);
	arg_types = new Type[arg_count];
	for (int i = arg_count;  --i >= 0; )
	  arg_types[i] = scmObjectType;
	if (lexp.isModuleBody ())
	  arg_types[0] = Compilation.scmEnvironmentType;
	argsArray = null;
      }

    ClassType superType
      = ClassType.make(lexp.isModuleBody () ? "kawa.lang.ModuleBody"
		       : "kawa.lang.Procedure" + arg_letter);
    curClass.setSuper (superType);

    Type[] constructor_args = apply0args;
    boolean constructor_takes_staticLink = false;
    if (lexp.staticLink != null)
      {
	lexp.staticLinkField = curClass.addField ("staticLink", objArrayType);
	if (lexp.outerLambda () != null)
	  {
	    constructor_args = applyNargs;
	    constructor_takes_staticLink = true;
	  }
      }

    Method constructor_method = curClass.addMethod ("<init>",
						     constructor_args,
						     Type.void_type,
						     Access.PUBLIC);
    curClass.constructor = constructor_method;
    Method superConstructor = superType.addMethod ("<init>",
						    apply0args, Type.void_type,
						    Access.PUBLIC);

    method = constructor_method;
    constructor_method.init_param_slots ();
    CodeAttr code = getCode();
    code.emitPushThis();
    code.emitInvokeSpecial(superConstructor);
    if (constructor_takes_staticLink)
      {
	code.emitPushThis();
	Variable staticLinkArg = code.getArg(1);
	code.emitLoad(staticLinkArg);
	code.emitPutField(lexp.staticLinkField);
      }

    // If immediate, we cannot set the function name in the constructor,
    // since setLiterals has not been called yet (ecept for nested functions).
    if (lexp.name != null && !immediate)
      {
	constructor_method.compile_push_this ();
	compileConstant (lexp.name);
	code.emitPutField(nameField);
      }
    code.emitReturn();

    if (arg_letter == 'N')
      {
	method = curClass.addMethod("numArgs", apply0args, Type.int_type,
				    Access.PUBLIC);
	method.init_param_slots ();
	code = getCode();
	code.emitPushInt(lexp.min_args | (lexp.max_args << 12));
	code.emitReturn();
      }


    String apply_name = lexp.isModuleBody () ? "run" : "apply"+arg_letter;
    Method apply_method
      = curClass.addMethod (apply_name, arg_types, scmObjectType,
			     Access.PUBLIC|Access.FINAL);
    method = apply_method;

    // If incomingMap[i] is non-null, it means that the user's i'th
    // formal parameter (numbering the left-most one as 0) is captured
    // by an inferior lambda, so it needs to be saved in the heapFrame.
    // The incoming variable is incomingMap[i], which is in register (i+1)
    // (since the unnamed "this" parameter is in register 0).
    Declaration incomingMap[] = new Declaration[lexp.min_args];

    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    int i = 0;
    method.initCode();
    code = getCode();

    Variable var = lexp.firstVar();
    if (var.getName() == "this")
      var.setType(new_class);
    else
       throw new Error("internal error - 'this' is not first arg");

    int line = lexp.getLine();
    if (line > 0)
      method.compile_linenumber (line);

    for ( ;  var != null;  var = var.nextVar ())
      {
	if (! (var instanceof Declaration) || ! var.isParameter ())
	  continue;
	// i is the register to use for the current parameter
	Declaration decl = (Declaration) var;
	if (var.isSimple ())
	  {
	    // For a simple parameter not captured by an inferior lambda,
	    // just allocate it in the incoming register.  This case also
	    // handles the artificial "this" and "argsArray" variables.
	    if (! var.isAssigned ()
		&& ! var.reserveLocal(i, code))
	      throw new Error ("internal error assigning parameters");
	  }
	else if (argsArray != null)
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
	    Declaration incoming = lexp.addDeclaration (incoming_name);
	    incoming.setArtificial (true);
	    incoming.setParameter (true);
	    if (! incoming.reserveLocal(i, code))
	      throw new Error ("internal error assigning parameters");
	    incoming.baseVariable = decl;
	    // Subtract 1, so we don't count the "this" variable.
	    incomingMap[i-1] = incoming;
	  }
	i++;
      }

    if (arg_letter == 'N'
	 // && If generating code to check number of arguments
	)
      {
	code.emitPushThis();
	code.emitLoad(argsArray);
	code.emitArrayLength();
	code.emitInvokeStatic(checkArgCountMethod);
      }

    code.enterScope (lexp.scope);

    if (lexp.heapFrame != null)
      {
	code.emitPushInt(lexp.frameSize);
	code.emitNewArray(scmObjectType);
	code.emitStore(lexp.heapFrame);
      }

    if (lexp.staticLink != null)
      {
	method.compile_push_this ();
	getCode().emitGetField(lexp.staticLinkField);
	SetExp.compile_store (lexp.staticLink, this);
      }

    // For each non-artificial parameter, copy it from its incoming
    // location (a local variable register, or the argsArray) into
    // its home location, if they are different.
    i = 0;
    int opt_i = 0;
    int key_i = 0;
    int key_args = lexp.keywords == null ? 0 : lexp.keywords.length;
    int opt_args = lexp.defaultArgs == null ? 0
      : lexp.defaultArgs.length - key_args;
    for (var = lexp.firstVar ();  var != null; var = var.nextVar ())
      {
	if (var.isParameter () && ! var.isArtificial ())
	  {
	    if (argsArray != null || incomingMap[i] != null)
	      {
		// If the parameter is captured by an inferior lambda,
		// then the incoming parameter needs to be copied into its
		// slot in the heapFrame.  Thus we emit an aaload instruction.
		// Unfortunately, it expects the new value *last*,
		// so first push the heapFrame array and the array index.
		Declaration param = (Declaration) var;
		if (!param.isSimple ())
		  {
		    ReferenceExp.compile_load (param.baseVariable, this);
		    code.emitPushInt(param.offset);
		  }
		// This part of the code pushes the incoming argument.
		if (argsArray == null)
		  {
		    // Simple case:  Incoming register is in incomingMap[i]:
		    code.emitLoad(incomingMap[i]);
		  }
		else if (i < lexp.min_args)
		  { // This is a required parameter, in argsArray[i].
		    code.emitLoad(argsArray);
		    code.emitPushInt(i);
		    code.emitArrayLoad(scmObjectType);
		  }
		else if (i < lexp.min_args + opt_args)
		  { // An optional parameter
		    code.emitPushInt(i);
		    code.emitLoad(argsArray);
		    code.emitArrayLength();
		    code.emitIfIntLt();
		    code.emitLoad(argsArray);
		    code.emitPushInt(i);
                    code.emitArrayLoad(scmObjectType);
		    code.emitElse();
		    lexp.defaultArgs[opt_i++].compile(this, 0);
		    code.emitFi();
		  }
		else if (lexp.max_args < 0 && i == lexp.min_args + opt_args)
		  {
		    // This is the "rest" parameter (i.e. following a "."):
		    // Convert argsArray[i .. ] to a list.
		    code.emitLoad(argsArray);
		    code.emitPushInt(i);
		    code.emitInvokeStatic(makeListMethod);
		  }
		else
		  { // Keyword argument.
		    if (searchForKeywordMethod == null)
		      {
			Type[] argts = new Type[3];
			argts[0] = objArrayType;
			argts[1] = Type.int_type;
			argts[2] = scmObjectType;
			searchForKeywordMethod
			  = scmKeywordType.addMethod("searchForKeyword",
						      argts, scmObjectType,
						      Access.PUBLIC|Access.STATIC);
		      }
		    code.emitLoad(argsArray);
		    code.emitPushInt(lexp.min_args + opt_args);
		    compileConstant(lexp.keywords[key_i++]);
		    code.emitInvokeStatic(searchForKeywordMethod);
		    code.emitDup(1);
		    code.emitGotoIf(199); // ifnonnull
		    code.emitPop(1);
		    lexp.defaultArgs[opt_i++].compile(this, 0);
		    code.emitFi();
		  }
		// Now finish copying the incoming argument into its
		// home location.
		if (param.isSimple ())
		  code.emitStore(param);
		else
		  code.emitArrayStore(Compilation.scmObjectType);
	      }
	    i++;
	  }
      }

    lexp.start_label = new Label (method);
    lexp.start_label.define (method);

    lexp.body.compile_with_linenumber (this, Expression.LAST);
    if (method.reachableHere ())
      code.emitReturn();

    if (! immediate && curClass == mainClass && literalsChain != null)
      {
	Method save_method = method;
	method = curClass.addMethod ("<clinit>", apply0args, Type.void_type,
				     Access.PUBLIC|Access.STATIC);
	method.init_param_slots ();
	dumpLiterals ();
	getCode().emitReturn();
	method = save_method;
      }

    method.popScope();
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
	code.emitInvokeSpecial(constructor_method);
	code.emitLoad(code.getArg(0));
	Method moduleMain
	  = superType.addMethod("runAsMain", Access.PUBLIC,
				args, Type.void_type);
	code.emitInvokeVirtual(moduleMain);
	code.emitReturn();
      }

    return new_class;
  }
}
