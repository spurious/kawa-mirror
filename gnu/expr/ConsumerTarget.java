// Copyright (c) 2000, 2001 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.OccurrenceType;

/**
 * A Target which is some variable that implements gnu.lists.Consumer.
 */

public class ConsumerTarget extends Target
{
  Variable consumer;
  boolean isContextTarget;

  public ConsumerTarget(Variable consumer)
  {
    this.consumer = consumer;
  }

  public Variable getConsumerVariable() { return consumer; }

  /** True iff this target is the current CallContext's current Consumer. */
  public final boolean isContextTarget () { return isContextTarget; }

  /** Make a Target that uses the current CallContext's current Consumer. */
  public static Target makeContextTarget (Compilation comp)
  {
    CodeAttr code = comp.getCode();
    comp.loadCallContext();
    code.emitGetField(Compilation.typeCallContext
		      .getDeclaredField("consumer"));
    Scope scope = code.getCurrentScope();
    Variable result
      = scope.addVariable(code, Compilation.typeConsumer, "$result");
    code.emitStore(result);
    ConsumerTarget target = new ConsumerTarget(result);
    target.isContextTarget = true;
    return target;
  }

  /** Compile an expression using a temporary Consumer, if needed. */
  public static void compileUsingConsumer(Expression exp,
					  Compilation comp, Target target)
  {
    if (target instanceof ConsumerTarget || target instanceof IgnoreTarget)
      exp.compile(comp, target);
    else
      {
	ClassType typeValues = Compilation.typeValues;
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
    compileFromStack(comp, stackType, -1);
  }

  /** Write stack value to Consumer.
   * @param consumerPushed if -1, then Consumer has not been pushed;
   *   if 1, Consumer was pushed before value, and value is a known singleton;
   *   if 0, Consumer was pushed before value, otherwise.
   */
  void compileFromStack(Compilation comp,
                        Type stackType, int consumerPushed)
  {
    CodeAttr code = comp.getCode();
    String methodName = null;
    Method method = null;
    Type methodArg = null;
    boolean islong = false;
    stackType = stackType.getImplementationType();
    char sig;
    if (stackType instanceof PrimType)
      {
	sig = stackType.getSignature().charAt(0);
	switch (sig)
	  {
	  case 'B': case 'S': case 'I':
	    methodName = "writeInt";
            methodArg = Type.intType;
            break;
	  case 'J':
            methodName = "writeLong";
            methodArg = Type.longType;
            islong = true;
            break;
	  case 'F':
            methodName = "writeFloat";
            methodArg = Type.floatType;
            break;
	  case 'D':
            methodName = "writeDouble";
            methodArg = Type.doubleType;
            islong = true;
            break;
	  case 'C':
            /* #ifdef JAVA5 */
            methodName = "append";
            methodArg = Type.charType;
            /* #else */
            // methodName = "write";
            // methodArg = Type.intType;
            /* #endif */
            break;
	  case 'Z':	
            methodName = "writeBoolean";
            methodArg = Type.booleanType;
            break;
	  case 'V':     return;
	  }
      }
    else
      {
        sig = '\0';
	if (consumerPushed == 1 || OccurrenceType.itemCountIsOne(stackType))
          {
            methodName = "writeObject";
            methodArg = Type.pointer_type;
          }
	else
	  {
	    method = (Compilation.typeValues
		      .getDeclaredMethod("writeValues", 2));
	    code.emitLoad(consumer);
            if (consumerPushed == 0)
              code.emitSwap();
	    code.emitInvokeStatic(method);
	    return;
	  }
      }
    if (consumerPushed >= 0)
      ;
    else if (islong)
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
      {
        Type[] methodArgs = { methodArg };
        method = Compilation.typeConsumer
          .getDeclaredMethod(methodName, methodArgs);
      }
    if (method != null)
      code.emitInvokeInterface(method);
    if (sig == 'C')
      code.emitPop(1); // Pop consumer result.
  }

  public boolean compileWrite (Expression exp, Compilation comp)
  {
    Type stackType = exp.getType();
    Type implType = stackType.getImplementationType();
    if ((implType instanceof PrimType && ! implType.isVoid())
        || gnu.kawa.reflect.OccurrenceType.itemCountIsOne(implType))
      {
        // Optimization to avoid a 'swap'.
        comp.getCode().emitLoad(this.consumer);
        Target starget = StackTarget.getInstance(implType);
        exp.compile(comp, starget);
        compileFromStack(comp, implType, 1);
        return true;
      }
    return false;
   }

  public Type getType() { return Compilation.scmSequenceType; }
}
