// Copyright (c) 2000, 2001 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/**
 * A Target which is some variable that implements gnu.lists.Consumer.
 */

public class ConsumerTarget extends Target
{
  Variable consumer;

  public ConsumerTarget(Variable consumer)
  {
    this.consumer = consumer;
  }

  public Variable getConsumerVariable() { return consumer; }

  /** Compile an expression using a temporary Consumer, if needed. */
  public static void compileUsingConsumer(Expression exp,
					  Compilation comp, Target target)
  {
    if (target instanceof ConsumerTarget || target instanceof IgnoreTarget)
      exp.compile(comp, target);
    else
      {
	CodeAttr code = comp.getCode();
	Scope scope = code.pushScope();
	Variable values = scope.addVariable(code, comp.typeValues, null);
	ConsumerTarget ctarget = new ConsumerTarget(values);
	code.emitInvokeStatic(comp.typeValues.getDeclaredMethod("make", 0));
	code.emitStore(values);
	exp.compile(comp, ctarget);
	code.emitLoad(values);
	code.popScope();
	target.compileFromStack(comp, Compilation.typeValues);
      }
  }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    CodeAttr code = comp.getCode();
    String methodName = null;
    Method method = null;
    if (stackType instanceof PrimType)
      {
	char sig = stackType.getSignature().charAt(0);
	switch (sig)
	  {
	  case 'B': case 'S': case 'I':
	    methodName = "writeInt";  break;
	  case 'J':	methodName = "writeLong";  break;
	  case 'F':	methodName = "writeFloat";  break;
	  case 'D':	methodName = "writeDouble";  break;
	  case 'C':	methodName = "writeChar";  break;
	  case 'Z':	methodName = "writeBoolean";  break;
	  case 'V':     return;
	  }
      }
    else
      {
	if (SeriesTarget.isSingletonType(stackType))
	  methodName = "writeObject";
	else
	  {
	    method = comp.typeValues.getDeclaredMethod("writeValues", 2);
	    code.emitLoad(consumer);
	    code.emitInvokeStatic(method);
	    return;
	  }
      }
    code.emitLoad(consumer);
    code.emitSwap();
    if (method == null && methodName != null)
      method = ClassType.make("gnu.lists.Consumer")
	.getDeclaredMethod(methodName, 1);
    if (method != null)
      code.emitInvokeInterface(method);
  }

  public Type getType() { return Compilation.scmSequenceType; }
}
