package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Implement the standard Scheme function "eq?". */

public class eq_p extends Procedure2 implements Inlineable
{
  Interpreter interpreter;

  public eq_p(Interpreter interpreter)
  {
    this.interpreter = interpreter;
  }

  public boolean apply(Object arg1, Object arg2)
  {
    return arg1 == arg2;
  }

  public Object apply2(Object arg1, Object arg2) 
  {
    return interpreter.booleanObject(arg1==arg2);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    compile(exp.getArgs(), comp, target, interpreter);
  }

  public static void compile (Expression[] args, Compilation comp,
			      Target target, Interpreter interpreter)
  {
    CodeAttr code = comp.getCode();
    args[0].compile(comp, Target.pushObject);
    args[1].compile(comp, Target.pushObject);
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	if (ctarget.trueBranchComesFirst)
	  {
	    code.emitGotoIfNE(ctarget.ifFalse);
	    code.emitGoto(ctarget.ifTrue);
	  }
	else
	  {
	    code.emitGotoIfEq(ctarget.ifTrue);
	    code.emitGoto(ctarget.ifFalse);
	  }
      }
    else
      {
	Type type;
	code.emitIfEq();
	if (target.getType() instanceof ClassType)
	  {
	    Object trueValue = interpreter.booleanObject(true);
	    Object falseValue = interpreter.booleanObject(false);
	    comp.compileConstant(trueValue, Target.pushObject);
	    code.emitElse();
	    comp.compileConstant(falseValue, Target.pushObject);
	    if (trueValue instanceof Boolean && falseValue instanceof Boolean)
	      type = Compilation.scmBooleanType;
	    else
	      type = Type.pointer_type;
	  }
	else
	  {
	    code.emitPushInt(1);
	    code.emitElse();
	    code.emitPushInt(0);
	    type = interpreter.getTypeFor(Boolean.TYPE);
	  }
	code.emitFi();
	target.compileFromStack(comp, type);
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return interpreter.getTypeFor(Boolean.TYPE);
  }
}
