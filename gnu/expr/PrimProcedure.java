// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import java.util.Hashtable;
import gnu.mapping.*;

/** A primitive Procedure implemented by a plain Java method. */

public class PrimProcedure extends MethodProc implements gnu.expr.Inlineable
{
  Type retType;
  Type[] argTypes;
  Method method;
  int op_code;

  /** If non-null, the LambdaExp that this PrimProcedure implements. */
  LambdaExp source;

  java.lang.reflect.Member member;

  public final int opcode() { return op_code; }

  public Type getReturnType () { return retType; }
  public void setReturnType (Type retType) { this.retType = retType; }

  public Type getReturnType (Expression[] args) { return retType; }

  /** Return true iff the last parameter is a "rest" argument. */
  public boolean takesVarArgs()
  {
    return method.getName().endsWith("$V");
  }

  /** Return a buffer that can contain decoded (matched) arguments. */
  public Object getVarBuffer()
  {
    return new Object[minArgs() + (takesVarArgs() ? 1 : 0)];
  }

  public int numArgs()
  {
    int num = argTypes.length;
    if (! getStaticFlag())
      num++;
    return takesVarArgs() ? (num - 1) + (-1 << 12) : num + (num << 12);
  }

  public RuntimeException match (Object vars, Object[] args)
  {
    int nargs = args.length;
    Object[] rargs = (Object[]) vars;
    boolean takesVarArgs = takesVarArgs();
    int fixArgs = takesVarArgs ? rargs.length - 1 : rargs.length;

    if (takesVarArgs ? nargs < fixArgs: nargs != rargs.length)
      return new WrongArguments(this, nargs);
    int arg_count = argTypes.length;
    Type elementType = null;
    Object[] restArray = null;
    if (takesVarArgs)
      {
	Type restType = argTypes[arg_count-1];
	if (restType == Compilation.scmListType)
	  {
	    rargs[rargs.length-1] = gnu.kawa.util.LList.makeList(args, fixArgs);
	    nargs = fixArgs;
	  }
	else
	  {
	    ArrayType restArrayType = (ArrayType) restType;
	    elementType = restArrayType.getComponentType();
	    Class elementClass = elementType.getReflectClass();
	    restArray = (Object[])
	      java.lang.reflect.Array.newInstance(elementClass, nargs-fixArgs);
	    rargs[rargs.length-1] = restArray;
	  }
      }
    int this_count = getStaticFlag() ? 0 : 1;
    if (this_count != 0)
      rargs[0] = method.getDeclaringClass().coerceFromObject(args[0]);
    for (int i = this_count;  i < nargs; i++)
      {
        try
          {
            Object arg = args[i];
            Type type = i < fixArgs ? argTypes[i-this_count] : elementType;
            if (type != Type.pointer_type)
              arg = type.coerceFromObject(arg);
            if (i < fixArgs)
              rargs[i] = arg;
            else
              restArray[i - fixArgs] = arg;
          }
        catch (ClassCastException ex)
          {
            return new WrongType(this, i, ex);
          }
      }
    return null;
  }

  public Object applyV (Object vars)
  {
    Object[] rargs = (Object[]) vars;
    int arg_count = argTypes.length;
    boolean is_constructor = op_code == 183;
    boolean is_static = getStaticFlag();

    try
      {
	if (member == null)
	  {
	    Class clas = method.getDeclaringClass().getReflectClass();
	    Class[] paramTypes = new Class[arg_count];
	    for (int i = arg_count; --i >= 0; )
	      paramTypes[i] = argTypes[i].getReflectClass();
	    if (is_constructor)
	      member = clas.getConstructor(paramTypes);
	    else
	      member = clas.getMethod(method.getName(), paramTypes);
	  }
	if (is_constructor)
	  return ((java.lang.reflect.Constructor) member).newInstance(rargs);
	else
	  {
	    java.lang.reflect.Method meth = (java.lang.reflect.Method) member;
	    Object result;
	    if (method.getStaticFlag())
	      result = meth.invoke(null, rargs);
	    else
              {
                Object[] pargs = new Object[arg_count];
                System.arraycopy(rargs, 1, pargs, 0, arg_count);
                result = meth.invoke(rargs[0], pargs);
              }
            return retType.coerceToObject(result);
	  }
      }
    catch (java.lang.reflect.InvocationTargetException ex)
      {
	Throwable th = ex.getTargetException();
	if (th instanceof RuntimeException)
	  throw (RuntimeException) th;
	if (th instanceof Error)
	  throw (Error) th;
	throw new RuntimeException(th.toString());
      }
    catch (Exception ex)
      {
	throw new RuntimeException("apply not implemented for PrimProcedure "+this+" - "+ ex);
      }
  }

  public PrimProcedure(java.lang.reflect.Method method,
		       Class thisClass, Class[] parameterClasses,
		       Interpreter interpreter)
  {
    Type[] parameterTypes = new Type[parameterClasses.length];
    for (int i = parameterClasses.length;  --i >= 0; )
      parameterTypes[i] = interpreter.getTypeFor(parameterClasses[i]);
    Type returnType = interpreter.getTypeFor(method.getReturnType());
    ClassType thisType = (ClassType) interpreter.getTypeFor(thisClass);
    init(thisType.addMethod(method.getName(), method.getModifiers(),
			    parameterTypes, returnType));
  }

  public PrimProcedure(java.lang.reflect.Method method,
		       Interpreter interpreter)
  {
    this(method, method.getDeclaringClass(),
	 method.getParameterTypes(), interpreter);
  }

  public PrimProcedure(Method method)
  {
    init(method);
  }

  private void init(Method method)
  {
    this.method = method;
    this.argTypes = method.getParameterTypes();
    this.retType = method.getReturnType();
    int flags = method.getModifiers();
    if ((flags & Access.STATIC) != 0)
      this.op_code = 184;  // invokestatic
    else
      {
	ClassType mclass = method.getDeclaringClass();
	if ((mclass.getModifiers() & Access.INTERFACE) != 0)
	  this.op_code = 185;  // invokeinterface
	else if ("<init>".equals(method.getName()))
	  this.op_code = 183;  // invokespecial
	else
	  this.op_code = 182;  // invokevirtual
      }
  }

  public PrimProcedure(Method method, LambdaExp source)
  {
    this(method);
    this.source = source;
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

  /** Compile arguments and push unto stack.
   * @param args arguments to evaluate and push.
   * @param thisType If we are calling a non-static function,
   *   then args[0] is the receiver and thisType is its expected class.
   *   If thisType==Type.void_type, ignore argType[0].
   *   If this_type==null, no special handling of args[0] or argTypes[0].
   */
  public static void compileArgs(Expression[] args,
				 Type thisType, Type[] argTypes,
				 boolean variable,
				 String name, LambdaExp source,
				 Compilation comp)
 {
    Type arg_type = null;
    gnu.bytecode.CodeAttr code = comp.getCode();
    int skipArg = thisType == Type.void_type ? 1 : 0;
    int arg_count = argTypes.length - skipArg;
    boolean is_static = thisType == null || skipArg != 0;
    int fix_arg_count = variable ? arg_count - (is_static ? 1 : 2)
      : args.length;
    for (int i = 0; ; ++i)
      {
        if (variable && i == fix_arg_count)
          {
            arg_type = argTypes[arg_count-1+skipArg];
	    if (arg_type == Compilation.scmListType)
	      {
		kawa.standard.list_v.compile(args, i, comp);
		break;
	      }
            code.emitPushInt(args.length - fix_arg_count);
            arg_type = ((ArrayType) arg_type).getComponentType();
            code.emitNewArray(arg_type);
          }
        if (i >= args.length)
          break;
        if (i >= fix_arg_count)
          {
            code.emitDup(1); // dup array.
            code.emitPushInt(i - fix_arg_count);
          }
        else
          arg_type = is_static ? argTypes[i + skipArg]
            : i==0 ? thisType
            : argTypes[i-1];
	args[i].compile(comp,
                        source == null
                        ? CheckedTarget.getInstance(arg_type, name, i)
                        : CheckedTarget.getInstance(arg_type, source, i));
        if (i >= fix_arg_count)
          code.emitArrayStore(arg_type);
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();

    if (opcode() == 183) // invokespecial == primitive-constructor
      {
	ClassType type = method.getDeclaringClass();
	code.emitNew(type);
	code.emitDup(type);
      }

    Expression[] args = exp.getArgs();
    String arg_error = WrongArguments.checkArgCount(this, args.length);
    if (arg_error != null)
      comp.error('e', arg_error);

    compile(getStaticFlag() ? null : method.getDeclaringClass(), args, comp, target);
  }

  public void compile (Type thisType, Expression[] args, Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    compileArgs(args, thisType, argTypes,
		takesVarArgs(), getName(), source, comp);
    
    if (method == null)
      code.emitPrimop (opcode(), args.length, retType);
    else
      code.emitInvokeMethod(method, opcode());

    target.compileFromStack(comp, retType);
  }

  public Type getParameterType(int index)
  {
    if (! getStaticFlag())
      {
        if (index == 0)
          return method.getDeclaringClass();
        index--;
      }
    int lenTypes = argTypes.length;
    if (index < lenTypes - 1)
      return argTypes[index];
    boolean varArgs = takesVarArgs();
    if (index < lenTypes && ! varArgs)
      return argTypes[index];
    // if (! varArgs) ERROR;
    return ((ArrayType) argTypes[lenTypes - 1]).getComponentType();
  }

  // This is null in JDK 1.1 and something else in JDK 1.2.
  private static ClassLoader systemClassLoader
  = PrimProcedure.class.getClassLoader();

  public static PrimProcedure getMethodFor (Procedure pproc, Expression[] args)
  {
    return getMethodFor(pproc, null, args, Interpreter.getInterpreter());
  }

  /** Search for a matching static method in a procedure's class.
   * @return a PrimProcedure that is suitable, or null. */
  public static PrimProcedure getMethodFor (Procedure pproc, Declaration decl,
					    Expression[] args,
					    Interpreter interpreter)
  {
    Class procClass;
    if (pproc instanceof gnu.expr.ModuleMethod)
      procClass = ((ModuleMethod) pproc).module.getClass();
    else
      procClass = pproc.getClass();
    if (procClass.getClassLoader() != systemClassLoader)
      return null;
    try
      {
        java.lang.reflect.Method[] meths = procClass.getDeclaredMethods();
        java.lang.reflect.Method best = null;
        Class[] bestTypes = null;
        String name = pproc.getName();
        if (name == null)
          return null;
        String mangledName
	  = decl == null || decl.field == null ? Compilation.mangleName(name)
	  : decl.field.getName();
        String mangledNameV = mangledName + "$V";
        for (int i = meths.length;  --i >= 0; )
          {
            java.lang.reflect.Method meth = meths[i];
            int mods = meth.getModifiers();
            if ((mods & (Access.STATIC|Access.PUBLIC))
                != (Access.STATIC|Access.PUBLIC))
	      {
		if (decl == null || decl.base == null)
		  continue;
	      }
            String mname = meth.getName();
            boolean variable;
            if (mname.equals("apply") || mname.equals(mangledName))
              variable = false;
            else if (mname.equals("apply$V") || mname.equals(mangledNameV))
              variable = true;
            else
              continue;
            Class[] ptypes = meth.getParameterTypes();
            if (variable ? ptypes.length - 1 > args.length
                : ptypes.length != args.length)
              continue;
            // In the future, we may try to find the "best" match.
            if (best != null)
              return null;
            best = meth;
            bestTypes = ptypes;
          }
        if (best != null)
          {
            PrimProcedure prproc = new PrimProcedure(best, procClass,
						     bestTypes, interpreter); 
	    prproc.setName(name);
            return prproc;
          }
      }
    catch (SecurityException ex)
      {
      }
    return null;
  }

  public String getName()
  {
    String name = super.getName();
    if (name != null)
      return name;
    name = getVerboseName();
    setName(name);
    return name;
  }

  public String getVerboseName()
  {
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
    return buf.toString();
  }


  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append(retType.getName());
    buf.append(' ');
    buf.append(getVerboseName());
    return buf.toString();
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<primitive procedure ");
    ps.print(toString());
    ps.print ('>');
  }
}
