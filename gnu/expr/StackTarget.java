// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.Values;

public class StackTarget extends Target
{
  Type type;
  public StackTarget(Type type) { this.type = type; }

  public Type getType() { return type; }

  public static Target getInstance(Type type)
  {
    return (type.isVoid() ? Target.Ignore
	    : type == Type.pointer_type ? Target.pushObject
            : new StackTarget(type));
  }

  protected boolean compileFromStack0(Compilation comp, Type stackType)
  {
    return compileFromStack0(comp, stackType, type);
  }

  static boolean compileFromStack0(Compilation comp, Type stackType, Type type)
  {
    if (type == stackType)
      return true;
    CodeAttr code = comp.getCode();
    if (stackType.isVoid())
      {
	comp.compileConstant (Values.empty);
	stackType = Type.pointer_type;
      }
    else if (stackType instanceof PrimType && type instanceof PrimType)
      {
	code.emitConvert(stackType, type);
	return true;
      }

    if (stackType instanceof ArrayType)
      {
	if (type == Type.pointer_type
	    || "java.lang.Cloneable".equals(type.getName()))
	  return true;
	// FIXME should check if stackType is compatible array type.
      }
    else
      {
	stackType.emitCoerceToObject(code);
	stackType = code.topType();
      }
    return ! CodeAttr.castNeeded(stackType, type);
  }

  public static void convert(Compilation comp, Type stackType, Type targetType)
  {
    if (! compileFromStack0(comp, stackType, targetType))
      emitCoerceFromObject(targetType, comp);
  }

  protected static void emitCoerceFromObject(Type type, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (type instanceof gnu.kawa.reflect.OccurrenceType)
      {
	// Kludge (OccurrenceType doesn't implement emitCoerceFromObject):
	comp.compileConstant(type, Target.pushObject);
	code.emitSwap();
	code.emitInvokeVirtual(ClassType.make("gnu.bytecode.Type")
			       .getDeclaredMethod("coerceFromObject", 1));
      }
    else
      {
	comp.usedClass(type);
	type.emitCoerceFromObject(code);
      }
  }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    if (! compileFromStack0(comp, stackType))
      emitCoerceFromObject(type, comp);
  }
}
