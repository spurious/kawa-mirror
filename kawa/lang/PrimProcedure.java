package kawa.lang;
import gnu.bytecode.*;
import java.util.Hashtable;
import gnu.math.IntNum;
import gnu.math.DFloNum;
import kawa.standard.Scheme;

/** A primitive Procedure implemented by a plain Java method. */

public class PrimProcedure extends ProcedureN implements Inlineable
{
  Type retType;
  Type[] argTypes;
  Method method;
  int op_code;

  java.lang.reflect.Member member;

  public final int opcode() { return op_code; }

  public int numArgs()
  {
    int num = argTypes.length;
    if (! getStaticFlag())
      num++;
    return num + (num << 12);
  }

  public Object applyN (Object[] args)
  {
    int arg_count = argTypes.length;
    boolean is_constructor = op_code == 183;
    ClassType this_type = method.getDeclaringClass();
    boolean is_static = getStaticFlag();
    int this_count = is_static ? 0 : 1;
    Procedure.checkArgCount(this, args.length);
    Object[] rargs = new Object[arg_count];
    for (int i = 0;  i < arg_count; i++)
      {
	rargs[i] = argTypes[i].coerceFromObject(args[i+this_count]);
      }
    try
      {
	if (member == null)
	  {
	    Class clas = this_type.getReflectClass();
	    Class[] paramTypes = new Class[arg_count];
	    for (int i = arg_count; --i >= 0; )
	      paramTypes[i] = argTypes[i].getReflectClass();
	    if (is_constructor)
	      member = clas.getConstructor(paramTypes);
	    else
	      member = clas.getMethod(method.getName(), paramTypes);
	  }
	if (is_constructor)
	  return ((java.lang.reflect.Constructor) member).newInstance(args);
	else
	  {
	    java.lang.reflect.Method meth = (java.lang.reflect.Method) member;
	    Object result;
	    if (method.getStaticFlag())
	      result = meth.invoke(null, args);
	    else
	      result = meth.invoke(this_type.coerceFromObject(args[0]), rargs);
	    return retType.coerceToObject(result);
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
	types.put ("int", Scheme.intType);
	types.put ("char", Scheme.charType);
	types.put ("boolean", Scheme.booleanType);
	types.put ("byte", Scheme.byteType);
	types.put ("short", Scheme.shortType);
	types.put ("long", Scheme.longType);
	types.put ("float", Scheme.floatType);
	types.put ("double", Scheme.doubleType);

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
    Procedure.checkArgCount(this, exp.args.length);
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
      code.emitInvokeMethod(method, opcode());

    if (retType == Type.void_type)
      {
	if ((flags & Expression.IGNORED) == 0)
	  comp.compileConstant (Interpreter.voidObject);
      }
    else if ((flags & Expression.IGNORED) != 0)
      code.emitPop(1);
    else
      retType.emitCoerceToObject(code);
  }

  public String getName()
  {
    String name = name();
    if (name != null)
      return name;
    StringBuffer buf = new StringBuffer(100);
    if (method == null)
      {
	buf.append("<op ");
	buf.append(op_code);
	buf.append('>');
      }
    else
      {
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
    name = buf.toString();
    setName(name);
    return name;
  }


  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append(retType.getName());
    buf.append(' ');
    buf.append(getName());
    return buf.toString();
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<primitive procedure ");
    ps.print(toString());
    ps.print ('>');
  }
}
