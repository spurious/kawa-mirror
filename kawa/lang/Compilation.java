package kawa.lang;
import codegen.*;
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

  // Various standard classes
  static public ClassType scmObjectType = new ClassType ("java.lang.Object");
  static public ClassType scmBooleanType = new ClassType ("java.lang.Boolean");
  static public ClassType scmSymbolType = new ClassType ("kawa.lang.Symbol");
  static public ClassType scmSequenceType = new ClassType ("kawa.lang.Sequence");
  static public ClassType javaStringType = new ClassType ("java.lang.String");
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
    = scmPairType.new_field ("car", scmObjectType, Access.PUBLIC);
  static final Field cdrField
    = scmPairType.new_field ("cdr", scmObjectType, Access.PUBLIC);
  static final Field trueConstant
    = scmInterpreterType.new_field ("trueObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC); 
  static final Field falseConstant
    = scmInterpreterType.new_field ("falseObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC);
  static final Field nullConstant
  = scmInterpreterType.new_field ("nullObject", scmListType,
				    Access.PUBLIC|Access.STATIC);
  static final Field voidConstant
  = scmInterpreterType.new_field ("voidObject", scmUndefinedType,
				    Access.PUBLIC|Access.STATIC);
  static final Field undefinedConstant
  = scmInterpreterType.new_field ("undefinedObject", scmUndefinedType,
				    Access.PUBLIC|Access.STATIC);
  static final Field eofConstant
  = scmSequenceType.new_field ("eofValue", scmSymbolType,
				    Access.PUBLIC|Access.STATIC);
  static final Field nameField
  = scmNamedType.new_field ("sym_name", scmSymbolType, Access.PUBLIC);
  static Method makeSymbolMethod;
  static Method initIntegerMethod;
  static Method lookupGlobalMethod;
  static Method defineGlobalMethod;
  static Method makeListMethod;
  static Type[] int1Args = { Type.int_type };
  static Type[] string1Arg = { javaStringType };

  static {
    Type[] makeListArgs = { objArrayType, Type.int_type };
    makeListMethod = scmListType.new_method ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
    initIntegerMethod = javaIntegerType.new_method ("<init>",
						    int1Args, Type.void_type,
						    Access.PUBLIC);

    makeSymbolMethod = scmSymbolType.new_method ("make", string1Arg,
						 scmSymbolType,
						 Access.PUBLIC|Access.STATIC);
    Type[] sym1Arg = { scmSymbolType };
    lookupGlobalMethod
      = scmEnvironmentType.new_method ("lookup_global", sym1Arg,
				       scmObjectType,
				       Access.PUBLIC|Access.STATIC);
    Type[] symObjArgs = { scmSymbolType, scmObjectType };
    defineGlobalMethod
      = scmEnvironmentType.new_method ("define_global", symObjArgs,
				       Type.void_type,
				       Access.PUBLIC|Access.STATIC);
  }

  static Type[] apply0args = Type.typeArray0;
  static Type[] applyNargs = { objArrayType };

  static final Method makeNullPairMethod
  = scmPairType.new_method ("makePair", apply0args, scmPairType,
			     Access.PUBLIC|Access.STATIC);
  static Method makePairMethod;

  public static Method apply0method = scmProcedureType.new_method
  ("apply0", apply0args, scmObjectType, Access.PUBLIC|Access.FINAL);

  public static Method apply1method;
  public static Method apply2method;
  public static Method apply3method;
  public static Method apply4method;
  public static Method applyNmethod;
  static Type[] apply1args = { scmObjectType };

  static
  {
    apply1method = scmProcedureType.new_method ("apply1", apply1args,
						scmObjectType,
						Access.PUBLIC|Access.FINAL);
    Type[] apply2args = { scmObjectType, scmObjectType };
    apply2method = scmProcedureType.new_method ("apply2", apply2args,
						scmObjectType,
						Access.PUBLIC|Access.FINAL);
    Type[] apply3args = { scmObjectType, scmObjectType, scmObjectType };
    apply3method = scmProcedureType.new_method ("apply3", apply3args,
						scmObjectType,
						Access.PUBLIC|Access.FINAL);
    Type[] apply4args = { scmObjectType , scmObjectType, scmObjectType, scmObjectType};
    apply4method = scmProcedureType.new_method ("apply4", apply4args,
						scmObjectType,
						Access.PUBLIC|Access.FINAL);
    applyNmethod = scmProcedureType.new_method ("applyN", applyNargs,
						scmObjectType,
						Access.PUBLIC|Access.FINAL);
    makePairMethod = scmPairType.new_method ("makePair", apply2args,
					     scmPairType,
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
	else if (value == Sequence.eofValue)
	  literal = new Literal (value, eofConstant, this);
	else if (value == Interpreter.nullObject)
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
    Literal literal = findLiteral (value);
    if (literal.field == null)
      literal.assign (this);
    literal.compile (this);
    return;
  }

  public void dumpLiterals ()
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

  /** If non-null: a prefix for generateClassNam to prepend to names. */
  public String classPrefix;

  /** Convert a string to a safe class name. */
  public static String mangleClassName (String name)
  {
    int len = name.length ();
    StringBuffer mangled = new StringBuffer (len);
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt (i);
	// This function is probably not quite enough ...
	// (Note the verifier may be picky about class names.)
	if (Character.isLowerCase (ch) || Character.isUpperCase (ch)
	    || Character.isDigit (ch))
	  mangled.append (ch);
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
    hint = mangleClassName (hint);
    if (classPrefix != null)
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
	  literalsField = new_class.new_field ("literals",
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
      = new ClassType (lexp.isModuleBody () ? "kawa.lang.ModuleBody"
		       : "kawa.lang.Procedure" + arg_letter);
    curClass.set_super (superType);

    Type[] constructor_args = apply0args;
    boolean constructor_takes_staticLink = false;
    if (lexp.staticLink != null)
      {
	lexp.staticLinkField = curClass.new_field ("staticLink", objArrayType);
	if (lexp.outerLambda () != null)
	  {
	    constructor_args = applyNargs;
	    constructor_takes_staticLink = true;
	  }
      }

    Method constructor_method = curClass.new_method ("<init>",
						     constructor_args,
						     Type.void_type,
						     Access.PUBLIC);
    curClass.constructor = constructor_method;
    Method superConstructor = superType.new_method ("<init>",
						    apply0args, Type.void_type,
						    Access.PUBLIC);

    constructor_method.init_param_slots ();
    constructor_method.compile_push_this ();
    constructor_method.compile_invoke_nonvirtual (superConstructor);
    if (constructor_takes_staticLink)
      {
	constructor_method.compile_push_this ();
	Variable staticLinkArg = constructor_method.find_arg (1);
	constructor_method.compile_push_value (staticLinkArg);
	constructor_method.compile_putfield (lexp.staticLinkField);
      }

    method = constructor_method;

    // If immediate, we cannot set the function name in the constructor,
    // since setLiterals has not been called yet (ecept for nested fnctions).
    if (lexp.name != null && !immediate)
      {
	constructor_method.compile_push_this ();
	compileConstant (lexp.name);
	constructor_method.compile_putfield (nameField);
      }

    String apply_name = lexp.isModuleBody () ? "run" : "apply"+arg_letter;
    Method apply_method
      = curClass.new_method (apply_name, arg_types, scmObjectType,
			     Access.PUBLIC|Access.FINAL);
    method = apply_method;


    // If imcomingMap[i] is non-null, it means that the user's i'th
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
    for (Variable var = lexp.firstVar ();  var != null;  var = var.nextVar ())
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
		&& ! method.assign_local (var, i))
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
	    Symbol incoming_name = Symbol.make (var.strName ()+"Incoming");
	    Declaration incoming = lexp.add_decl (incoming_name);
	    incoming.setArtificial (true);
	    incoming.setParameter (true);
	    if (! method.assign_local (incoming, i))
	      throw new Error ("internal error assigning parameters");
	    incoming.baseVariable = decl;
	    // Subtract 1, so we don't count the "this" variable.
	    incomingMap[i-1] = incoming;
	  }
	i++;
      }

    method.enterScope (lexp.scope);

    if (lexp.heapFrame != null)
      {
	method.compile_push_int (lexp.frameSize);
	method.compile_new_array (scmObjectType);
	method.compile_store_value (lexp.heapFrame);
      }

    if (lexp.staticLink != null)
      {
	method.compile_push_this ();
	method.compile_getfield (lexp.staticLinkField);
	SetExp.compile_store (lexp.staticLink, this);
      }

    // For each non-artificial parameter, copy it from its incoming
    // location (a local variable register, or the argsArray) into
    // its home location, if they are different.
    i = 0;
    for (Variable var = lexp.firstVar ();  var != null; var = var.nextVar ())
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
		    method.compile_push_int (param.offset);
		  }
		// This part of the code pushes the incoming argument.
		if (argsArray == null)
		  {
		    // Simple case:  Incoming register is in incomingMap[i]:
		    method.compile_push_value (incomingMap[i]);
		  }
		else
		  {
		    // Incoming parameters are in argsArray.
		    method.compile_push_value (argsArray);
		    method.compile_push_int (i);
		    if (i >= lexp.min_args)
		      {
			// This is the "rest" parameter (i.e. following a "."):
			// Convert argsArray[i .. ] to a list.
			method.compile_invoke_static (makeListMethod);
		      }
		    else
		      {
			// This is a required parameter, in argsArray[i].
			method.compile_array_load (scmObjectType);
		      }
		  }
		// Now finish copying the incoming argument into its
		// home location.
		if (param.isSimple ())
		  method.compile_store_value (param);
		else
		  method.compile_array_store (Compilation.scmObjectType);
	      }
	    i++;
	  }
      }

    lexp.start_label = new Label (method);
    lexp.start_label.define (method);

    lexp.body.compile_with_linenumber (this, Expression.LAST);
    if (method.reachableHere ())
      method.compile_return ();

    if (! immediate && curClass == mainClass)
      {
	Method save_method = method;
	method = constructor_method;
	dumpLiterals ();
	method = save_method;
      }
    constructor_method.compile_return ();

    method.pop_scope ();
    curLambda = saveLambda;

    return new_class;
  }
}
