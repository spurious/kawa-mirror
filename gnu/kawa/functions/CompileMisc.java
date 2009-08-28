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
  static final int NOT = 3;
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

  public static CompileMisc forNot(Object proc)
  {
    return new CompileMisc((Procedure) proc, NOT);
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
      case NOT:
        return inlineNot((Not) proc, exp,
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
      case NOT:
        compileNot((Not) proc, exp, comp, target);
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
      case NOT:
        return ((Not) proc).language.getTypeFor(Boolean.TYPE);
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

  public static Expression inlineNot (Not proc, ApplyExp exp,
                                      InlineCalls walker,
                                      boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    return exp.inlineIfConstant(proc, walker);
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

  public void compileNot (Not proc, ApplyExp exp, Compilation comp, Target target)
  {
    Expression arg = exp.getArgs()[0];
    Language language = proc.language;
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	ConditionalTarget sub_target
	  = new ConditionalTarget(ctarget.ifFalse, ctarget.ifTrue, language);
	sub_target.trueBranchComesFirst = ! ctarget.trueBranchComesFirst;
	arg.compile(comp, sub_target);
	return;
      }
    CodeAttr code = comp.getCode();
    Type type = target.getType();
    if (target instanceof StackTarget && type.getSignature().charAt(0) == 'Z')
      {
	arg.compile(comp, target);
	code.emitNot(target.getType());
      }
    else
      {
        QuoteExp trueExp = QuoteExp.getInstance(language.booleanObject(true));
        QuoteExp falseExp = QuoteExp.getInstance(language.booleanObject(false));
	IfExp.compile(arg, falseExp, trueExp, comp, target);
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
