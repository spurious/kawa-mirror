package gnu.expr;

import gnu.bytecode.Type;

public class TailTarget extends StackTarget
{
  public TailTarget(Type type) { super(type); }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    if (! comp.curLambda.isHandlingTailCalls())
      super.compileFromStack(comp, stackType);
    else if (comp.method.reachableHere())
      {
	gnu.bytecode.CodeAttr code = comp.getCode();
	Target.pushObject.compileFromStack(comp, stackType);
	code.emitLoad(comp.callStackContext);
	code.emitSwap();
	code.emitPutField(comp.valueCallStackField);
	code.emitReturn();
      }
  }
}
