// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.Values;

/** Same as StackTarget, but catch ClassCastException.
 * Generate code so that if coercion fails, catch ClassCastException,
 * and re-throw a WrongType.  This gives better error messages. */

public class CheckedTarget extends StackTarget
{
  LambdaExp proc;
  String procname;
  int argno;

  public CheckedTarget(Type type, LambdaExp proc, int argno)
  {
    super(type);
    this.proc = proc;
    this.procname = proc.getName();
    this.argno = argno;
  }

  public CheckedTarget(Type type, String procname, int argno)
  {
    super(type);
    this.procname = procname;
    this.argno = argno;
  }

  public static Target getInstance(Type type, String procname, int argno)
  {
    return (type == Type.pointer_type ? Target.pushObject
            : new CheckedTarget(type, procname, argno));
  }

  public static Target getInstance(Type type, LambdaExp proc, int argno)
  {
    return (type == Type.pointer_type ? Target.pushObject
            : new CheckedTarget(type, proc, argno));
  }

  static ClassType typeClassCastException;
  static ClassType typeWrongType;
  static Method makeWrongTypeStringMethod;
  static Method makeWrongTypeProcMethod;

  private static void initWrongType()
  {
    if (typeClassCastException == null)
      typeClassCastException = ClassType.make("java.lang.ClassCastException");
    if (typeWrongType == null)
      {
        typeWrongType= ClassType.make("gnu.mapping.WrongType");
        Type[] args = new Type[3];
        args[0] = typeClassCastException;
        args[1] = Compilation.javaStringType;
        args[2] = Type.int_type;
        makeWrongTypeStringMethod
          = typeWrongType.addMethod("make", args,
                                    typeWrongType,
                                    Access.PUBLIC|Access.STATIC);
        args = new Type[3];
        args[0] = typeClassCastException;
        args[1] = Compilation.typeProcedure;
        args[2] = Type.int_type;
        makeWrongTypeProcMethod
          = typeWrongType.addMethod("make", args,
                                    typeWrongType,
                                    Access.PUBLIC|Access.STATIC);
      }
  }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    if (! compileFromStack0(comp, stackType))
      emitCheckedCoerce(comp, proc, procname, argno, type);
  }

  public static void emitCheckedCoerce(Compilation comp,
                                       String procname, int argno, Type type)
  {
    emitCheckedCoerce(comp, null, procname, argno, type);
  }

  public static void emitCheckedCoerce(Compilation comp, LambdaExp proc,
                                       int argno, Type type)
  {
    emitCheckedCoerce(comp, proc, proc.getName(), argno, type);
  }

  static void emitCheckedCoerce(Compilation comp, LambdaExp proc,
                                String procname, int argno, Type type)
  {
    CodeAttr code = comp.getCode();
    // If we're not in a try statement, it is more efficient to defer
    // the handler to the end of the procedure (thus avoiding a goto,
    // and potentially improving locality).  If we're in a try statement,
    // we can't safely do that
    boolean isInTry = code.isInTry();
    initWrongType();
    int startPC = code.getPC();
    emitCoerceFromObject(type, comp);

    int endPC = code.getPC();
    // If no cast was needed, no code has been generated.
    // Thus endPC is equal to startPC and we can stop safely.
    if (endPC == startPC)
      return;

    // Can never raise an exception, so we don't need to catch it.
    if (type == Type.tostring_type)
      return;

    Label endLabel = null;
    if (isInTry)
      {
        endLabel = new Label(code);
        code.emitGoto(endLabel);
	endPC = code.getPC();
      }
    code.addHandler(startPC, endPC, isInTry ? endPC : -1,
                    typeClassCastException,
                    code.getConstants());
    if (! isInTry)
      code.beginFragment(true);
    // Push arguments:
    // ClassCastException is already pushed
    code.pushType(typeClassCastException);
    boolean thisIsProc = false;
    if (proc != null && proc.isClassGenerated()
        && ! comp.method.getStaticFlag())
      {
        if (comp.method.getDeclaringClass() == proc.getCompiledClassType(comp))
          thisIsProc = true;
      }
    int line = comp.getLine();
    if (line > 0)
      code.putLineNumber(line);
    if (thisIsProc)
      code.emitPushThis();
    else
      code.emitPushString(procname == null ? "lambda" : procname);
    code.emitPushInt(argno);
    code.emitInvokeStatic(thisIsProc ? makeWrongTypeProcMethod
                          : makeWrongTypeStringMethod);
    code.emitThrow();
    if (isInTry)
      endLabel.define(code);
    else
      code.endFragment();
  }
}
