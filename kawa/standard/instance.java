package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;

public class instance extends Procedure2 implements Inlineable
{
  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = (Type) arg2;
    return type.isInstance(arg1) ? Boolean.TRUE : Boolean.FALSE;
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method instanceMethod;

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    CodeAttr code = comp.getCode();
    Type type = Scheme.getTypeValue(args[1]);
    if (type != null)
      {
	args[0].compile(comp, Target.pushObject);
	type.emitIsInstance(code);
      }
    else
      {
	if (typeType == null)
	  {
	    typeType = ClassType.make("gnu.bytecode.Type");
	    instanceMethod = typeType.addMethod("isInstance",
						Compilation.apply1args,
						Scheme.booleanType,
						gnu.bytecode.Access.PUBLIC);
	  }
	args[1].compile(comp, typeType);
	args[0].compile(comp, Target.pushObject);
	code.emitInvokeVirtual(instanceMethod);
      }
    target.compileFromStack(comp, Scheme.booleanType);
  }
}
