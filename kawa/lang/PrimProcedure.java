package kawa.lang;
import gnu.bytecode.*;
import java.util.Hashtable;
import gnu.math.IntNum;
import gnu.math.DFloNum;

/** A primitive Procedure implemented by a plain Java method. */

public class PrimProcedure extends ProcedureN implements Inlineable
{
  Type retType;
  Type[] argTypes;
  Method method;
  int op_code;

  java.lang.reflect.Member member;

  public final int opcode() { return op_code; }

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	if (member == null)
	  {
	    Class clas = method.getDeclaringClass().getReflectClass();
	    int i = argTypes.length;
	    Class[] paramTypes = new Class[i];
	    while (--i >= 0)
	      paramTypes[i] = argTypes[i].getReflectClass();
	    if (op_code == 183)
	      member = clas.getConstructor(paramTypes);
	    else
	      member = clas.getMethod(method.getName(), paramTypes);
	  }
	if (member instanceof java.lang.reflect.Constructor)
	  return ((java.lang.reflect.Constructor) member).newInstance(args);
	else
	  {
	    java.lang.reflect.Method meth = (java.lang.reflect.Method) member;
	    if (method.getStaticFlag())
	      return meth.invoke(null, args);
	    else
	      {
		Object[] rargs = new Object[args.length - 1];
		System.arraycopy(args, 1, rargs, 0, args.length - 1);
		return meth.invoke(args[0], rargs);
	      }
	  }
      }
    catch (Exception ex)
      {
	throw new GenericError("apply not implemented for PrimProcedure - " + ex);
      }
  }

  public PrimProcedure(Method method)
  {
    this.method = method;
    this.argTypes = method.getParameterTypes();
    this.retType = method.getReturnType();
  }

  public PrimProcedure(int opcode, Type retType, Type[] argTypes)
  {
    this.op_code = opcode;
    this.retType = retType;
    this.argTypes= argTypes;
  }

  public PrimProcedure(int op_code, ClassType classtype, String name,
		       Type retType, Type[] argTypes)
  {
    this.op_code = op_code;
    if (op_code == 185) // invokeinterface
      classtype.access_flags |= Access.INTERFACE;
    method = classtype.addMethod (name, argTypes, retType,
				   op_code == 184 ? Access.STATIC : 0);
    this.retType = retType;
    this.argTypes= argTypes;
  }

  /** Use to compile new followed by constructor. */
  public PrimProcedure(ClassType classtype, Type[] argTypes)
  {
    this(183, classtype, "<init>", Type.void_type, argTypes);
    this.retType = classtype;
  }

  public final boolean getStaticFlag()
  {
    return method == null || method.getStaticFlag() || op_code == 183;
  }

  public final Type[] getParameterTypes() { return argTypes; }

  static Hashtable types;

  public static Type getNamedType (String name)
  {
    if (types == null)
      {
	types = new Hashtable ();
	types.put ("void", Type.void_type);
	types.put ("int", Type.int_type);
	types.put ("char", Type.char_type);
	types.put ("boolean", Type.boolean_type);
	types.put ("byte", Type.byte_type);
	types.put ("short", Type.short_type);
	types.put ("long", Type.long_type);
	types.put ("float", Type.float_type);
	types.put ("double", Type.double_type);

	types.put ("Object", Type.pointer_type);
	types.put ("java.lang.Object", Type.pointer_type);
	types.put ("String", Type.string_type);
	types.put ("java.lang.String", Type.string_type);

	types.put ("object", Type.pointer_type);
	types.put ("integer", new ClassType("gnu.math.IntNum"));
	types.put ("symbol", new ClassType("java.lang.String"));
	types.put ("keyword", new ClassType("kawa.lang.Keyword"));
	types.put ("list", new ClassType("kawa.lang.List"));
	types.put ("pair", new ClassType("kawa.lang.Pair"));
	types.put ("string", new ClassType("kawa.lang.FString"));
	types.put ("vector", new ClassType("kawa.lang.Vector"));
	types.put ("function", new ClassType("kawa.lang.Procedure"));
      }
    return (Type) types.get(name);
  }

  public static Type string2Type (String name)
  {
    Type t = getNamedType (name);
    if (t != null)
      return t;
    if (name.endsWith("[]"))
      {
	t = string2Type(name.substring(0, name.length()-2));
	t = new ArrayType(t);
      }
    else
      t = new ClassType (name);
    types.put (name, t);
    return t;
  }

  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    int arg_count = argTypes.length;
    boolean is_static = getStaticFlag();
    if (exp.args.length != arg_count + (is_static ? 0 : 1))
      throw new Error ("internal error - wrong number of arguments to primitive");
    if (opcode() == 183) // invokespecial == primitive-constructor
      {
	ClassType type = method.getDeclaringClass();
	code.emitNew(type);
	code.emitDup(type);
      }
    for (int i = 0; i < exp.args.length; ++i)
      {
	Type arg_type = is_static ? argTypes[i]
	  : i==0 ? method.getDeclaringClass()
	  : argTypes[i-1];
	exp.args[i].compile (comp, 0, arg_type);
      }
    
    if (method == null)
      comp.method.compile_primop (opcode(), exp.args.length, retType);
    else
      comp.method.compile_invoke_method (method, opcode());

    if (retType == Type.void_type)
      {
	if ((flags & Expression.IGNORED) == 0)
	  comp.compileConstant (Interpreter.voidObject);
      }
    else if ((flags & Expression.IGNORED) != 0)
      code.emitPop(1);
    else if (retType instanceof ClassType)
      return;
    else if (retType instanceof ArrayType)
      return;  // ??? Should we convert to FVector?
    else if (retType == Type.int_type || retType == Type.short_type
	     || retType == Type.byte_type)
      {
	IntNum.initMakeMethods();
	comp.method.compile_invoke_static (IntNum.makeIntMethod);
      }
    else if (retType == Type.long_type)
      {
	IntNum.initMakeMethods();
	comp.method.compile_invoke_static (IntNum.makeLongMethod);
      }
    else if (retType == Type.double_type || retType == Type.float_type)
      {
	comp.method.compile_convert (Type.double_type);
	DFloNum.initMakeMethods();
	comp.method.compile_invoke_static (DFloNum.makeMethod);
      }
    else if (retType == Type.char_type)
      {
	Char.initMakeMethods();
	comp.method.compile_invoke_static (Char.makeCharMethod);
      }
    else if (retType == Type.boolean_type)
      {
	comp.method.compile_if_neq_0 ();
	comp.compileConstant (Interpreter.trueObject);
	code.emitElse();
	comp.compileConstant (Interpreter.falseObject);
	code.emitFi();
      }
    else
      throw new Error ("unimplemented return type");
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append(retType.getName());
    if (method == null)
      {
	buf.append("<op ");
	buf.append(op_code);
	buf.append('>');
      }
    else
      {
	buf.append(' ');
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

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<primitive procedure ");
    ps.print(toString());
    ps.print ('>');
  }
}
