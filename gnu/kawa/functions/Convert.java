package gnu.kawa.functions;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.CodeAttr;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.Invoke;

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

  public Expression inline (ApplyExp exp, InlineCalls walker)
  {
    exp.walkArgs(walker);
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

	args[0].compile(comp, typeType);
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

  /**
   * Convenience method to make an Expression that coerces a value.
   * @param value to be coerced
   * @param type to coerce value to
   * @return expression that coerces value to type
   */
  public static Expression makeCoercion(Expression value, Expression type)
  {
    Expression[] exps = new Expression[2];
    exps[0] = type;
    exps[1] = value;
    QuoteExp c = new QuoteExp(Convert.getInstance());
    return new ApplyExp(c, exps);
  }

  /**
   * Convenience method to make an Expression that coerces a value.
   * @param value to be coerced
   * @param type to coerce value to
   * @return expression that coerces value to type
   */
  public static Expression makeCoercion(Expression value, Type type)
  {
    return makeCoercion(value, new QuoteExp(type));
  }

  /** Modify LambdaExp so result is coerced to given type. */
  public static void setCoercedReturnValue (LambdaExp lexp, Expression type,
					    Language language)
  {
    gnu.bytecode.Type rtype = language.getTypeFor(type);
    if (rtype != null)
      {
	Expression value = lexp.body;
	lexp.body = Convert.makeCoercion(value, type);
	lexp.body.setLine(value);
	lexp.setReturnType(rtype);
      }
  }
}
