package kawa.lang;

import gnu.bytecode.*;

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
	comp.compileConstant (Interpreter.voidObject);
	stackType = Type.pointer_type;
      }
    else if (stackType instanceof PrimType && type instanceof PrimType)
      {
	code.emitConvert(type);
	return;
      }
    stackType.emitCoerceToObject(code);
    type.emitCoerceFromObject(code);
  }
}
