// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.WrongType;

/** Same as StackTarget, but catch ClassCastException.
 * Generate code so that if coercion fails, catch ClassCastException,
 * and re-throw a WrongType.  This gives better error messages. */

public class CheckedTarget extends StackTarget
{
  LambdaExp proc;
  String procname;
  /** 1-origin argument index,
      or WrongType.ARG_CAST, or WrongType-ARG_VARNAME. */
  int argno;

  public CheckedTarget(Type type)
  {
    super(type);
    argno = WrongType.ARG_CAST;
  }

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

  public static Target getInstance(Type type)
  {
    return (type == Type.pointer_type ? Target.pushObject
            : new CheckedTarget(type));
  }

  static ClassType typeClassCastException;
  static ClassType typeWrongType;
  static Method initWrongTypeStringMethod;
  static Method initWrongTypeProcMethod;

  private static void initWrongType()
  {
    if (typeClassCastException == null)
      typeClassCastException = ClassType.make("java.lang.ClassCastException");
    if (typeWrongType == null)
      {
        typeWrongType= ClassType.make("gnu.mapping.WrongType");
        Type[] args = new Type[4];
        args[0] = typeClassCastException;
        args[1] = Compilation.javaStringType;
        args[2] = Type.intType;
	args[3] = Type.pointer_type;
        initWrongTypeStringMethod
          = typeWrongType.addMethod("<init>", Access.PUBLIC,
                                    args, Type.voidType);
        args = new Type[4];
        args[0] = typeClassCastException;
        args[1] = Compilation.typeProcedure;
        args[2] = Type.intType;
	args[3] = Type.pointer_type;
        initWrongTypeProcMethod
          = typeWrongType.addMethod("<init>", Access.PUBLIC,
                                    args, Type.voidType);
      }
  }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    if (! compileFromStack0(comp, stackType))
      emitCheckedCoerce(comp, proc, procname, argno, type, null);
  }

  public static void emitCheckedCoerce(Compilation comp,
                                       String procname, int argno, Type type)
  {
    emitCheckedCoerce(comp, null, procname, argno, type, null);
  }

  public static void emitCheckedCoerce(Compilation comp, LambdaExp proc,
                                       int argno, Type type)
  {
    emitCheckedCoerce(comp, proc, proc.getName(), argno, type, null);
  }

  public static void emitCheckedCoerce(Compilation comp, LambdaExp proc,
                                       int argno, Type type, Variable argValue)
  {
    emitCheckedCoerce(comp, proc, proc.getName(), argno, type, argValue);
  }

  static void emitCheckedCoerce(Compilation comp, LambdaExp proc,
                                String procname, int argno, Type type,
				Variable argValue)
  {
    CodeAttr code = comp.getCode();
    // If we're not in a try statement, it is more efficient to defer
    // the handler to the end of the procedure (thus avoiding a goto,
    // and potentially improving locality).  If we're in a try statement,
    // we can't safely do that
    boolean isInTry = code.isInTry();
    initWrongType();
    Label startTry = new Label(code);
    Scope tmpScope;
    if (argValue == null && type != Type.tostring_type)
      {
	tmpScope = code.pushScope();
	argValue = code.addLocal(Type.pointer_type);
	code.emitDup(1);
	code.emitStore(argValue);
      }
    else
      tmpScope = null;
    int startPC = code.getPC();
    startTry.define(code);
    emitCoerceFromObject(type, comp);

    int endPC = code.getPC();
    // If no cast was needed, no code has been generated.
    // Thus endPC is equal to startPC and we can stop safely.
    // Also, tostring_type can never raise an exception, so we don't need
    // to catch it.
    if (endPC == startPC
	|| type == Type.tostring_type)
      {
        // FIXME should remove generated store to argValue, by truncating
        // PC to startPC.  But take care with startPC.position.
	if (tmpScope != null)
	  code.popScope();
	return;
      }

    Label endTry = new Label(code);
    endTry.define(code);

    Label endLabel = new Label(code);
    if (isInTry)
      code.emitGoto(endLabel);
    int fragment_cookie = 0;
    if (! isInTry)
      fragment_cookie = code.beginFragment(new Label(code), endLabel);
    code.addHandler(startTry, endTry, typeClassCastException);
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
    int line = comp.getLineNumber();
    if (line > 0)
      code.putLineNumber(line);
    code.emitNew(typeWrongType);
    code.emitDupX(); // dup_x1
    code.emitSwap();
    if (thisIsProc)
      code.emitPushThis();
    else
      code.emitPushString(procname == null && argno != WrongType.ARG_CAST
			  ? "lambda"
			  : procname);
    code.emitPushInt(argno);
    code.emitLoad(argValue);
    code.emitInvokeSpecial(thisIsProc ? initWrongTypeProcMethod
                           : initWrongTypeStringMethod);
    if (tmpScope != null)
      code.popScope();
    code.emitThrow();
    if (isInTry)
      endLabel.define(code);
    else
      code.endFragment(fragment_cookie);
  }
}
