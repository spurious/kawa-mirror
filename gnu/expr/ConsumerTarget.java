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
	ClassType typeValues = comp.typeValues;
	compileUsingConsumer(exp, comp, target,
			     typeValues.getDeclaredMethod("make", 0),
			     typeValues.getDeclaredMethod("canonicalize", 0));
      }
  }

  public static void compileUsingConsumer (Expression exp, Compilation comp,
					   Target target,
					   Method makeMethod,
					   Method resultMethod)
  {
    CodeAttr code = comp.getCode();
    Scope scope = code.pushScope();
    Type ctype;
    if (makeMethod.getName() == "<init>")
      {
	ClassType cltype = makeMethod.getDeclaringClass();
	ctype = cltype;
	code.emitNew(cltype);
	code.emitDup(ctype);
	code.emitInvoke(makeMethod);
      }
    else
      {
	ctype = makeMethod.getReturnType();
	code.emitInvokeStatic(makeMethod);
      }
    Variable consumer = scope.addVariable(code, ctype, null);
    ConsumerTarget ctarget = new ConsumerTarget(consumer);
    code.emitStore(consumer);
    exp.compile(comp, ctarget);
    code.emitLoad(consumer);
    if (resultMethod != null)
      code.emitInvoke(resultMethod);
    code.popScope();
    target.compileFromStack(comp, resultMethod == null ? ctype
			    : resultMethod.getReturnType());
  }


  public void compileFromStack(Compilation comp, Type stackType)
  {
    CodeAttr code = comp.getCode();
    String methodName = null;
    Method method = null;
    boolean islong = false;
    if (stackType instanceof PrimType)
      {
	char sig = stackType.getSignature().charAt(0);
	switch (sig)
	  {
	  case 'B': case 'S': case 'I':
	    methodName = "writeInt";  break;
	  case 'J':	methodName = "writeLong";  islong = true; break;
	  case 'F':	methodName = "writeFloat";  break;
	  case 'D':	methodName = "writeDouble"; islong = true; break;
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
    if (islong)
      {
	code.pushScope();
	Variable temp = code.addLocal(stackType);
	code.emitStore(temp);
	code.emitLoad(consumer);
	code.emitLoad(temp);
	code.popScope();
      }
    else
      {
	code.emitLoad(consumer);
	code.emitSwap();
      }
    if (method == null && methodName != null)
      method = Compilation.typeConsumer.getDeclaredMethod(methodName, 1);
    if (method != null)
      code.emitInvokeInterface(method);
  }

  public Type getType() { return Compilation.scmSequenceType; }
}
