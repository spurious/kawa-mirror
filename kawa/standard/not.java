package kawa.standard;
import gnu.bytecode.Type;
import gnu.bytecode.CodeAttr;
import gnu.mapping.*;
import gnu.expr.*;

/** Implement the standard Scheme procedure "not". */

public class not extends Procedure1 implements Inlineable
{
  Interpreter interpreter;
  public QuoteExp trueExp;
  public QuoteExp falseExp;

  public not(Interpreter interpreter)
  {
    this.interpreter = interpreter;
    trueExp = new QuoteExp(interpreter.booleanObject(true));
    falseExp = new QuoteExp(interpreter.booleanObject(false));
  }

  public not(Interpreter interpreter, String name)
  {
    this(interpreter);
    setName(name);
  }

  public Object apply1 (Object arg1)
   {
     return interpreter.booleanObject(! interpreter.isTrue(arg1));
   }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression arg = exp.getArgs()[0];
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	ConditionalTarget sub_target
	  = new ConditionalTarget(ctarget.ifFalse, ctarget.ifTrue, interpreter);
	sub_target.trueBranchComesFirst = ! ctarget.trueBranchComesFirst;
	arg.compile(comp, sub_target);
	return;
      }
    CodeAttr code = comp.getCode();
    Type type = target.getType();
    if (target instanceof StackTarget && type.getSignature().charAt(0) == 'Z')
      {
	arg.compile(comp, target);
	code.emitNot(target.getType());
      }
    else
      {
	IfExp.compile(arg, falseExp, trueExp, comp, target);
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return interpreter.getTypeFor(Boolean.TYPE);
  }
}
