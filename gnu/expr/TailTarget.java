package gnu.expr;
import gnu.bytecode.*;
import gnu.lists.*;

public class TailTarget extends StackTarget
{
  public TailTarget(Type type) { super(type); }

  public static Target getInstance(Type type)
  {
    return (type == Type.pointer_type ? Target.returnObject
            : new TailTarget(type));
  }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    if (! comp.curLambda.isHandlingTailCalls())
      super.compileFromStack(comp, stackType);
    else if (comp.method.reachableHere())
      {
	String sig = stackType.getSignature();
	char sig0 = sig.charAt(0);
	if (sig0 == 'V')
	  return;
	gnu.bytecode.CodeAttr code = comp.getCode();
	Target.pushObject.compileFromStack(comp, stackType);
	code.emitLoad(comp.callStackContext);
	code.emitSwap();
	code.emitInvokeVirtual(writeValueMethod);
	code.emitReturn();
      }
  }

  static Method writeValueMethod
  = Compilation.typeCallContext.getDeclaredMethod("writeValue", 1);
}
