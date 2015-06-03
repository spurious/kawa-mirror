// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.Values;
import gnu.kawa.reflect.LazyType;

public class StackTarget extends Target
{
  Type type;
  public StackTarget(Type type) { this.type = type; }

    /** Target is field or array element that automatically truncates.
     * Relevant if type is (signed or unsigned) byte or short, and
     * the given type is an int.  Hence we can avoid the truncation.
     */
    protected boolean autoTruncates;

    boolean autoTruncates(Type stackType) {
        if (autoTruncates && stackType == Type.intType) {
            char sig1 = type.getSignature().charAt(0);
            if (sig1 == 'B' || sig1 == 'S' || sig1 == 'C')
                return true;
        }
        return false;
    }

  public Type getType() { return type; }

  public static Target getInstance(Type type)
  {
    return (type.isVoid() ? Target.Ignore
	    : type == Type.pointer_type ? Target.pushObject
            : new StackTarget(type));
  }

    public static Target getTruncatingInstance(Type type) {
        if (type.isVoid())
            return Target.Ignore;
        if (type == Type.pointer_type)
            return Target.pushObject;
        StackTarget target = new StackTarget(type);
        target.autoTruncates = true;
        return target;
    }

    protected StackTarget getClonedInstance(Type type) {
        return new StackTarget(type);
    }

    public static void forceLazyIfNeeded(Compilation comp, Type stackType, Type type) {
	if (LazyType.maybeLazy(stackType) && ! LazyType.maybeLazy(type))
            forceLazy(comp, stackType, type);
    }

    public static Type forceLazy(Compilation comp, Type stackType, Type type) {
        Type rawType = type.getRawType();
        if (rawType.compare(stackType.getRawType()) < 0) {
            Method forceMethod;
            if (stackType instanceof LazyType) {
                forceMethod = LazyType.lazyType.getDeclaredMethod("getValue", 0);
            } else {
                int nargsforce;
                if (rawType instanceof ClassType) {
                    nargsforce = 2;
                    comp.loadClassRef((ClassType) rawType);
                } else
                    nargsforce = 1;
                forceMethod = ClassType.make("gnu.mapping.Promise")
                    .getDeclaredStaticMethod("force", nargsforce);
            }
            CodeAttr code = comp.getCode();
            code.emitInvoke(forceMethod);
            stackType = stackType instanceof LazyType ? ((LazyType) stackType).getValueType() : Type.objectType;
        }
        return stackType;
    }

  protected boolean compileFromStack0(Compilation comp, Type stackType)
  {
    return compileFromStack0(comp, stackType, type);
  }

  static boolean compileFromStack0(Compilation comp, Type stackType, Type type)
  {
    CodeAttr code = comp.getCode();
    forceLazyIfNeeded(comp, stackType, type);

    if (type == stackType || ! code.reachableHere())
      return true;
    if (stackType.isVoid())
      {
	comp.compileConstant (Values.empty);
	stackType = Type.pointer_type;
      }
    else if (stackType instanceof PrimType && type instanceof PrimType)
      {
          code.emitConvert((PrimType) stackType, (PrimType) type);
	return true;
      }

    if (stackType instanceof ArrayType)
      {
	if (type == Type.pointer_type
	    || "java.lang.Cloneable".equals(type.getName()))
	  return true;
      }
    else
      {
        type.emitConvertFromPrimitive(stackType, code);
	stackType = code.topType();
      }
    return type.isCompatibleWithValue(stackType) > 1;
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

    public void compileFromStack(Compilation comp, Type stackType) {
        if (type instanceof LazyType && ! (stackType instanceof LazyType)) {
            LazyType ltype = (LazyType) type;
            if (! LazyType.maybeLazy(stackType))
                getClonedInstance(ltype.getValueType()).compileFromStack(comp, stackType);
            Method wrapMethod = ClassType.make("gnu.mapping.Promise").getDeclaredStaticMethod("coerceToLazy", 1);
            comp.getCode().emitInvokeStatic(wrapMethod);
            comp.getCode().emitCheckcast(ltype.getRawType());
        }
        else if (! autoTruncates(stackType)
                 && ! compileFromStack0(comp, stackType))
            doCoerce(comp);
    }

    protected void doCoerce(Compilation comp) {
        emitCoerceFromObject(type, comp);
    }
}
