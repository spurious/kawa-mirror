// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/** Cause a class value from a ClassExp to be initialized. */

public class ClassInitializer extends Initializer
{
  ClassExp cexp;

  public ClassInitializer(ClassExp cexp, Compilation comp)
  {
    field = cexp.allocFieldFor(comp);
    this.cexp = cexp;
    if (field.getStaticFlag())
      {
	next = comp.clinitChain;
	comp.clinitChain = this;
      }
    else
      {
	LambdaExp heapLambda = LambdaExp.getHeapLambda(cexp.outer);
	next = heapLambda.initChain;
	heapLambda.initChain = this;
      }
  }

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (! field.getStaticFlag())
      code.emitPushThis();
    cexp.compile(comp, Target.pushValue(Compilation.typeClassType));
    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }
}
