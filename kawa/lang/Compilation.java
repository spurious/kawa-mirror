package kawa.lang;
import codegen.*;
import java.util.Hashtable;

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
  static public ClassType javaStringType = new ClassType ("java.lang.String");
  static public ClassType scmListType = new ClassType ("kawa.lang.List");
  static public ClassType scmUndefinedType = new ClassType ("kawa.lang.Undefined");
  static Type objArrayType = new ArrayType (scmObjectType);
  static public ClassType scmProcedureType
    = new ClassType ("kawa.lang.Procedure");
  static public ClassType scmInterpreterType
    = new ClassType ("kawa.lang.Interpreter");
  static Field trueConstant
    = scmInterpreterType.new_field ("trueObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC); 
  static Field falseConstant
    = scmInterpreterType.new_field ("falseObject", scmBooleanType,
				    Access.PUBLIC|Access.STATIC);
  static Field nullConstant
  = scmInterpreterType.new_field ("nullObject", scmObjectType, // FIXME type
				    Access.PUBLIC|Access.STATIC);
  static Field voidConstant
  = scmInterpreterType.new_field ("voidObject", scmUndefinedType,
				    Access.PUBLIC|Access.STATIC);
  static Field undefinedConstant
  = scmInterpreterType.new_field ("undefinedObject", scmUndefinedType,
				    Access.PUBLIC|Access.STATIC);
  static Field eofConstant
  = scmInterpreterType.new_field ("eofObject", scmSymbolType,
				    Access.PUBLIC|Access.STATIC);
  static Method makeSymbolMethod;
  static Method lookupGlobalMethod;
  static Method defineGlobalMethod;
  static Method makeListMethod;

  static {
    Type[] makeListArgs = { objArrayType, Type.int_type };
    makeListMethod = scmListType.new_method ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
    Type[] string1Arg = { javaStringType };
    makeSymbolMethod = scmSymbolType.new_method ("make", string1Arg,
						 scmSymbolType,
						 Access.PUBLIC|Access.STATIC);
    lookupGlobalMethod
      = scmInterpreterType.new_method ("lookup_global", string1Arg,
				       scmObjectType,
				       Access.PUBLIC|Access.STATIC);
    Type[] stringObjArgs = { javaStringType, scmObjectType };
    defineGlobalMethod
      = scmInterpreterType.new_method ("define_global", stringObjArgs,
				       Type.void_type,
				       Access.PUBLIC|Access.STATIC);
  }

  static Type[] apply0args = new Type[0];
  static Type[] applyNargs = { objArrayType };

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
  }

  public static Method[] applymethods = {
    apply0method, apply1method, apply2method, apply3method,
    apply4method, applyNmethod };

  Hashtable literalTable;
  int literalsCount;
  Literal literalsChain;
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

  public Compilation (ClassType classfile)
  {
    addClass (classfile);
    this.curClass = classfile;
    this.mainClass = classfile;

    literalTable = new Hashtable (100);
    literalsField = classfile.new_field ("literals",
					 objArrayType, Access.STATIC);
  }

  public void compileConstant (Object value)
  {
    if (value instanceof Boolean)
      {
	boolean val = ((Boolean)value).booleanValue ();
	method.compile_getstatic (val ? trueConstant : falseConstant);
	return;
      }
    if (value == Interpreter.voidObject)
      {
	method.compile_getstatic (voidConstant);
	return;
      }
    if (value == Interpreter.eofObject)
      {
	method.compile_getstatic (eofConstant);
	return;
      }
    if (value == Interpreter.nullObject)
      {
	method.compile_getstatic (nullConstant);
	return;
      }
    if (value == Interpreter.undefinedObject)
      {
	method.compile_getstatic (undefinedConstant);
	return;
      }
    if (immediate)
      {
	Literal literal = (Literal) literalTable.get (value);
	if (literal == null)
	  literal = new Literal (value, this);
	literal.compile (this);
	return;
      }
    if (value instanceof Symbol)
      {
	method.compile_push_string (((Symbol)value).toString ());
	method.compile_invoke_static (makeSymbolMethod);
	return;
      }
    System.err.print ("Unimplemented compileConstant for ");
    System.err.println (getClass ());
    method.compile_push_null ();
  }

  public Compilation (String classname)
  {
    this (new ClassType (classname));
  }
}
