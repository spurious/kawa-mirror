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
  static public ClassType scmStringType = new ClassType ("java.lang.StringBuffer");
  static public ClassType javaStringType = new ClassType ("java.lang.String");
  static public ClassType javaIntegerType = new ClassType ("java.lang.Integer");
  static public ClassType scmListType = new ClassType ("kawa.lang.List");
  static public ClassType scmPairType = new ClassType ("kawa.lang.Pair");
  static public ClassType scmUndefinedType = new ClassType ("kawa.lang.Undefined");
  static ArrayType objArrayType = new ArrayType (scmObjectType);
  static public ClassType scmProcedureType
    = new ClassType ("kawa.lang.Procedure");
  static public ClassType scmInterpreterType
    = new ClassType ("kawa.lang.Interpreter");
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
  = scmInterpreterType.new_field ("eofObject", scmSymbolType,
				    Access.PUBLIC|Access.STATIC);
  static Method makeSymbolMethod;
  static Method initIntegerMethod;
  static Method lookupGlobalMethod;
  static Method defineGlobalMethod;
  static Method makeListMethod;
  static Method initStringBufferMethod;
  static Type[] int1Args = { Type.int_type };

  static {
    Type[] makeListArgs = { objArrayType, Type.int_type };
    makeListMethod = scmListType.new_method ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
    initIntegerMethod = javaIntegerType.new_method ("<init>",
						    int1Args, Type.void_type,
						    Access.PUBLIC);

    Type[] string1Arg = { javaStringType };
    makeSymbolMethod = scmSymbolType.new_method ("make", string1Arg,
						 scmSymbolType,
						 Access.PUBLIC|Access.STATIC);
    initStringBufferMethod = scmStringType.new_method ("<init>", string1Arg,
						       Type.void_type,
						       Access.PUBLIC);

    Type[] sym1Arg = { scmSymbolType };
    lookupGlobalMethod
      = scmInterpreterType.new_method ("lookup_global", sym1Arg,
				       scmObjectType,
				       Access.PUBLIC|Access.STATIC);
    Type[] symObjArgs = { scmSymbolType, scmObjectType };
    defineGlobalMethod
      = scmInterpreterType.new_method ("define_global", symObjArgs,
				       Type.void_type,
				       Access.PUBLIC|Access.STATIC);
  }

  static Type[] apply0args = new Type[0];
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

  static
  {
    Type[] apply1args = { scmObjectType };
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

  public void addClass (ClassType new_class)
  {
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

  Literal findLiteral (Object value)
  {
    Literal literal = (Literal) literalTable.get (value);
    if (literal != null)
      {
	// This value is used multiple types (perhaps recursively),
	// so do allocate a LitN Field for it.
	if (literal.field == null)
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
	else if (value == Interpreter.eofObject)
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
	else
	  literal = new Literal (value, scmObjectType, this);
      }
    return literal;
  }

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

  public Compilation (LambdaExp lexp, String classname, boolean immediate)
  {
    ClassType classfile = new ClassType (classname);
    addClass (classfile);
    this.curClass = classfile;
    this.mainClass = classfile;
    this.immediate = immediate;

    literalTable = new Hashtable (100);
    if (immediate)
      literalsField = classfile.new_field ("literals",
                                           objArrayType, Access.STATIC);
    compilefunc.compile (this, lexp);
  }
}
