package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.Values;

public class StackTarget extends Target
{
  Type type;
  public StackTarget(Type type) { this.type = type; }

  public Type getType() { return type; }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    if (type == stackType)
      return;
    CodeAttr code = comp.getCode();
    if (stackType == Type.void_type)
      {
	comp.compileConstant (Values.empty);
	stackType = Type.pointer_type;
      }
    else if (stackType instanceof PrimType && type instanceof PrimType)
      {
	code.emitConvert(type);
	return;
      }
    if (stackType instanceof ClassType && type instanceof ClassType)
      {
	// If stackType inherits from target type, no coercion is needed.
	ClassType baseClass = (ClassType) stackType;
	while (baseClass != null)
	  {
	    if (baseClass == type)
	      return;
	    baseClass = baseClass.getSuperclass();
	  }
      }
    stackType.emitCoerceToObject(code);
    type.emitCoerceFromObject(code);
  }
}
