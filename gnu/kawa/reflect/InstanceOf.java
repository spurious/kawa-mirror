package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class InstanceOf extends Procedure2 implements CanInline, Inlineable
{
  Interpreter interpreter;

  public InstanceOf(Interpreter interpreter)
  {
    this.interpreter = interpreter;
  }

  public InstanceOf(Interpreter interpreter, String name)
  {
    this.interpreter = interpreter;
    setName(name);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = interpreter.asType(arg2);
    return interpreter.booleanObject(type.isInstance(arg1));
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method instanceMethod;

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    return Invoke.inlineClassName(exp, 1, interpreter);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    CodeAttr code = comp.getCode();
    Type type = null;
    Expression typeArg = args[1];
    if (typeArg instanceof QuoteExp)
      {
        try
          {
            type = interpreter.asType(((QuoteExp) typeArg).getValue());
          }
        catch (Exception ex)
          {
            comp.error('w', "unknown type spec: "+type);
          }
      }
    else 
      type = interpreter.getTypeFor(typeArg);
    if (type != null)
      {
	args[0].compile(comp, Target.pushObject);
	if (type instanceof TypeValue)
	  {
	    ((TypeValue) type).emitIsInstance(null, comp, target);
	    return;
	  }
	else
	  type.emitIsInstance(code);
      }
    else
      {
	if (typeType == null)
	  {
	    typeType = ClassType.make("gnu.bytecode.Type");
	    instanceMethod = typeType.addMethod("isInstance",
						Compilation.apply1args,
						Type.boolean_type,
						gnu.bytecode.Access.PUBLIC);
	  }
	args[1].compile(comp, typeType);
	args[0].compile(comp, Target.pushObject);
	code.emitInvokeVirtual(instanceMethod);
      }
    target.compileFromStack(comp, interpreter.getTypeFor(Boolean.TYPE));
  }

  public Type getReturnType (Expression[] args)
  {
    return interpreter.getTypeFor(Boolean.TYPE);
  }

  public static void emitIsInstance(TypeValue type, Variable incoming,
				    Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    type.emitTestIf(null, null, comp);
    ConditionalTarget cond = null;
    if (target instanceof ConditionalTarget)
      {
	cond = (ConditionalTarget) target;
	code.emitGoto(cond.ifTrue);
      }
    else
      code.emitPushInt(1);
    code.emitElse();
    if (cond != null)
      code.emitGoto(cond.ifFalse);
    else
      code.emitPushInt(0);
    code.emitFi();
    if (cond == null)
      target.compileFromStack(comp,
			      comp.getInterpreter().getTypeFor(Boolean.TYPE));
  }
}
