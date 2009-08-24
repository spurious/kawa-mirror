package gnu.kawa.functions;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.CodeAttr;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.lispexpr.LangObjType;

public class Convert extends Procedure2 implements CanInline, Inlineable
{
  public static final Convert as = new Convert();
  static { as.setName("as"); }

  public static Convert getInstance ()
  {
    return as;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    Type type;
    if (arg1 instanceof Class)
      type = Type.make((Class) arg1);
    else
      type = (Type) arg1;
    return type.coerceFromObject (arg2);
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method coerceMethod;

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    return Invoke.inlineClassName(exp, 0, walker);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      throw new Error ("wrong number of arguments to "+getName());
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

  public Type getReturnType (Expression[] args)
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
