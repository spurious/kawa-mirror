// Copyright (c) 2000, 2001 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/** The value in the result (as a sequence of values) is passed to a function.
 */

public class SeriesTarget extends Target
{
  /** Where to place each value. */
  public Variable value;

  /** A function to call (using jsr/jsr_w). */
  public Label function;

  /** Where to go when done. */
  public Label done;

  public void compileFromStackSimple(Compilation comp, Type stackType)
  {
    CodeAttr code = comp.getCode();
    StackTarget.convert(comp, stackType, value.getType());
    code.emitStore(value);
    code.emitJsr(function);
  }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    CodeAttr code = comp.getCode();

    if (isSingletonType(stackType))
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
    code.pushScope();
    Variable indexVar = code.addLocal(Type.int_type);
    Variable valuesVar = code.addLocal(Type.pointer_type);
    Variable nextVar = code.addLocal(Type.int_type); 
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
    code.emitGotoIfIntLtZero(done);
    code.emitLoad(valuesVar);
    code.emitLoad(indexVar);
    code.emitInvokeStatic(Compilation.typeValues.getDeclaredMethod("nextValue", 2));
    compileFromStackSimple(comp, Type.pointer_type);
    code.emitLoad(nextVar);
    code.emitStore(indexVar);
    code.emitGoto(top);
    code.pushScope();
    

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

  public Type getType() { return Type.pointer_type; }

  public static boolean isSingletonType (Type type)
  {
    if (type instanceof PrimType)
      return ! type.isVoid();
    if (type instanceof ArrayType)
      return true;
    if (type instanceof ClassType)
      {
	int cmp = type.compare(Compilation.typeValues);
	if (cmp == -3)
	  return true;
      }
    return false;
  }

}
