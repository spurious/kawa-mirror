package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.CodeAttr;
import gnu.mapping.*;
import gnu.expr.*;

public class convert extends Procedure2 implements Inlineable
{
  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = (Type) arg1;
    return type.coerceFromObject (arg2);
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method coerceMethod;

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      throw new Error ("wrong number of arguments to "+name());
    CodeAttr code = comp.getCode();
    Type type = Scheme.getTypeValue(args[0]);
    if (type != null)
      {
	args[1].compile(comp, Target.pushObject);
	type.emitCoerceFromObject(comp.getCode());
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
}
