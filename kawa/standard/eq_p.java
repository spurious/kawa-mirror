package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Implement the standard Scheme function "eq?". */

public class eq_p extends Procedure2 implements Inlineable {

  public Object apply2(Object arg1, Object arg2) 
  {
    if (arg1==arg2)
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    compile(exp.getArgs(), comp, target);
  }

  public static void compile (Expression[] args,
                              Compilation comp, Target target)
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
	    code.emitGetStatic(Compilation.trueConstant);
	    code.emitElse();
	    code.emitGetStatic(Compilation.falseConstant);
	    type = Compilation.scmBooleanType;
	  }
	else
	  {
	    code.emitPushInt(1);
	    code.emitElse();
	    code.emitPushInt(0);
	    type = Scheme.booleanType;
	  }
	code.emitFi();
	target.compileFromStack(comp, type);
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return Scheme.booleanType;
  }
}
