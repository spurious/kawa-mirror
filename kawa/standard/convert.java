package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.CodeAttr;

public class convert extends Procedure2 implements Inlineable
{
  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = (Type) arg1;
    return type.coerceFromObject (arg2);
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method coerceMethod;

  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      throw new Error ("wrong number of arguments to "+name());
    CodeAttr code = comp.getCode();
    if (args[0] instanceof QuoteExp)
      {
	Object arg0 = ((QuoteExp) args[0]).getValue();
	if (arg0 instanceof Type)
	  {
	    args[1].compile(comp, 0);
	    ((Type) arg0).emitCoerceFromObject(comp.getCode());

	  }
	else
	  throw new Error ("2nd arg is not a Type");
      }
    else
      {
	if (typeType == null)
	  {
	    typeType = new ClassType("gnu.bytecode.Type");
	    coerceMethod = typeType.addMethod("coerceFromObject",
					      Compilation.apply1args,
					      Type.pointer_type,
					      gnu.bytecode.Access.PUBLIC);
	  }

	args[0].compile(comp, 0, typeType);
	args[1].compile(comp, 0);
	code.emitInvokeVirtual(coerceMethod);
      }
    if ((flags & Expression.IGNORED) != 0)
      code.emitPop(1);
  }
}
