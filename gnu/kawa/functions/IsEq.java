package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Implement the standard Scheme function <tt>eq?</tt>
 * and the Lisp <tt>eq</tt>. */

public class IsEq extends Procedure2 implements Inlineable
{
  Language language;

  public IsEq(Language language, String name)
  {
    this.language = language;
    setName(name);
  }

  public boolean apply(Object arg1, Object arg2)
  {
    return arg1 == arg2;
  }

  public Object apply2(Object arg1, Object arg2) 
  {
    return language.booleanObject(arg1==arg2);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    compile(exp.getArgs(), comp, target, language);
  }

  public static void compile (Expression[] args, Compilation comp,
			      Target target, Language language)
  {
    CodeAttr code = comp.getCode();
    args[0].compile(comp, Target.pushObject);
    args[1].compile(comp, Target.pushObject);
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	if (ctarget.trueBranchComesFirst)
	  code.emitGotoIfNE(ctarget.ifFalse);
	else
	  code.emitGotoIfEq(ctarget.ifTrue);
	ctarget.emitGotoFirstBranch(code);
      }
    else
      {
	Type type;
	code.emitIfEq();
	if (target.getType() instanceof ClassType)
	  {
	    Object trueValue = language.booleanObject(true);
	    Object falseValue = language.booleanObject(false);
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
	    type = language.getTypeFor(Boolean.TYPE);
	  }
	code.emitFi();
	target.compileFromStack(comp, type);
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return language.getTypeFor(Boolean.TYPE);
  }
}
