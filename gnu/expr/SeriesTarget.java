// Copyright (c) 2000, 2001, 2006 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.OccurrenceType;
import gnu.kawa.reflect.SingletonType;

/** The value in the result (as a sequence of values) is passed to a function.
 */

public class SeriesTarget extends Target
{
  /** Where to place each value. */
  public Declaration param;

  /** A function to call (using jsr/jsr_w). */
  public Label function;

  /** Where to go when done executing the Expression whose target this is.
   * If null, execution should continue just after the Expression. */
  public Label done;

  /** A surrounding Scope for local Variables.
   * This Scope should include both any calls to compileFromStackSimple
   * and the entirety of the 'function' subroutine. This is protect against
   * where a variable logically goes out of scope, but we cannot re-use
   * the local variable slot until we're past the 'function'. */
  public Scope scope;

  public void compileFromStackSimple(Compilation comp, Type stackType)
  {
    CodeAttr code = comp.getCode();
    StackTarget.convert(comp, stackType, param.getType());
    param.compileStore(comp);
    code.emitJsr(function);
    if (done != null && code.reachableHere())
       code.emitGoto(done);
    // Make sure we don't free the local variable slots for any variable
    // slots prematurely.  I.e. any local variables in use at this point
    // must be protected from being re-used in the Jsr subroutine.
    code.locals.preserveVariablesUpto(scope);
  }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    CodeAttr code = comp.getCode();

    if (OccurrenceType.itemCountIsOne(stackType))
      {
	compileFromStackSimple(comp, stackType);
	return;
      }

    /* emit the following:
       int index = 0;
       for (;;)
       {
         int next = Values.nextIndex(values, index);
	 if (next < 0)
	   goto done;
	 Values.nextValue(values, index);
	 compileFromStackSimple(comp, Type.pointerType);
	 index = value;
       }
    */
    Variable indexVar = code.addLocal(Type.int_type);
    Variable valuesVar = code.addLocal(Type.pointer_type);
    Variable nextVar = code.addLocal(Type.int_type); 
    Label doneLabel = done;
    boolean doneGiven;
    if (doneLabel == null)
      {
        doneGiven = false;
        doneLabel = new Label(code);
      }
    else
      {
        doneGiven = true;
        done = null;  // To suppress goto in compileFromStackSimple.
      }
    StackTarget.convert(comp, stackType, Type.pointer_type);
    code.emitStore(valuesVar);
    code.emitPushInt(0);
    code.emitStore(indexVar);

    Label top = new Label(code);
    top.define(code);
    code.emitLoad(valuesVar);
    code.emitLoad(indexVar);
    code.emitInvokeStatic(Compilation.typeValues.getDeclaredMethod("nextIndex", 2));
    code.emitDup(Type.int_type);
    code.emitStore(nextVar);
    code.emitGotoIfIntLtZero(doneLabel);
    code.emitLoad(valuesVar);
    code.emitLoad(indexVar);
    code.emitInvokeStatic(Compilation.typeValues.getDeclaredMethod("nextValue", 2));
    compileFromStackSimple(comp, SingletonType.getInstance());
    code.emitLoad(nextVar);
    code.emitStore(indexVar);
    code.emitGoto(top);
    if (doneGiven)
      done = doneLabel;
    else
      doneLabel.define(code);

    /*
    if (stackType is singleton type)
      compileFromStackSimple(comp, stackType);
    else
      {
	code.emitDup(stackType);
	emit[if TOP instanceof Values];
	emit loop [Values, get, ...];
	code.emitElse();
	compileFromStackSimple(comp, stackType);
	code.emitFi();
      }
    */
  }

  public String toString()
  {
    return "SeriesTarget[param: "+param+"; func:"+function+" done:"+done+"]";
  }

  public Type getType() { return Type.pointer_type; }
}
