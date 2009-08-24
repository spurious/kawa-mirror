package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.lispexpr.LangObjType;
import kawa.standard.Scheme;

public class CompileMisc implements CanInline, Inlineable
{
  static final int CONSTANT_FUNCTION0 = 1;
  static final int CONVERT = 2;
  int code;
  Procedure proc;

  public CompileMisc(Procedure proc, int code)
  {
    this.proc = proc;
    this.code = code;
  }

  public static CompileMisc forConstantFunction0(Object proc)
  {
    return new CompileMisc((Procedure) proc, CONSTANT_FUNCTION0);
  }

  public static CompileMisc forConvert(Object proc)
  {
    return new CompileMisc((Procedure) proc, CONVERT);
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    switch (code)
      {
      case CONSTANT_FUNCTION0:
        return inlineConstantFunction0((ConstantFunction0) proc, exp,
                                       walker, argsInlined);
      case CONVERT:
        return inlineConvert((Convert) proc, exp,
                             walker, argsInlined);
      default: throw new Error();
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    switch (code)
      {
      case CONVERT:
        compileConvert((Convert) proc, exp, comp, target);
        return;
      default: throw new Error();
      }
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    switch (code)
      {
      case CONVERT:
        return getReturnTypeConvert((Convert) proc, args);
      default: throw new Error();
      }
  }

  public static Expression inlineConstantFunction0 (ConstantFunction0 proc,
                                                    ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    int nargs = exp.getArgCount();
    if (nargs != 0 && walker != null)
      {
	String message = WrongArguments.checkArgCount(proc, nargs);
	return walker.noteError(message);
      }
    return proc.constant;
  }

  public static Expression inlineConvert (Convert proc, ApplyExp exp,
                                   InlineCalls walker,
                                   boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    return Invoke.inlineClassName(exp, 0, walker);
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method coerceMethod;

  public static void compileConvert (Convert proc, ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      throw new Error ("wrong number of arguments to "+proc.getName());
    CodeAttr code = comp.getCode();
    Type type = Scheme.getTypeValue(args[0]);
    if (type != null)
      {
        args[1].compile(comp, Target.pushValue(type));
	if (code.reachableHere())
	  target.compileFromStack(comp, type);
      }
    else
      {
	if (typeType == null)
	  {
	    typeType = ClassType.make("gnu.bytecode.Type");
          }
        if (coerceMethod == null)
          {
	    coerceMethod = typeType.addMethod("coerceFromObject",
					      Compilation.apply1args,
					      Type.pointer_type,
					      gnu.bytecode.Access.PUBLIC);
	  }
	args[0].compile(comp, LangObjType.typeClassType);
	args[1].compile(comp, Target.pushObject);
	code.emitInvokeVirtual(coerceMethod);
	target.compileFromStack(comp, Type.pointer_type);
      }
  }

  public static Type getReturnTypeConvert (Convert proc, Expression[] args)
  {
    if (args != null && args.length == 2)
      {
	Type type = Scheme.getTypeValue(args[0]);
	if (type != null)
	  return type;
      }
    return Type.pointer_type;
  }
}
