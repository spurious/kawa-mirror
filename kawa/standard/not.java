package kawa.standard;
import gnu.bytecode.Type;
import gnu.bytecode.CodeAttr;
import gnu.mapping.*;
import gnu.expr.*;

/** Implement the standard Scheme procedure "not". */

public class not extends Procedure1 implements CanInline, Inlineable
{
  Language language;
  public QuoteExp trueExp;
  public QuoteExp falseExp;

  public not(Language language)
  {
    this.language = language;
    trueExp = new QuoteExp(language.booleanObject(true));
    falseExp = new QuoteExp(language.booleanObject(false));
  }

  public not(Language language, String name)
  {
    this(language);
    setName(name);
  }

  public Object apply1 (Object arg1)
   {
     return language.booleanObject(! language.isTrue(arg1));
   }

  
  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    return exp.inlineIfConstant(this, walker);
  }


  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression arg = exp.getArgs()[0];
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	ConditionalTarget sub_target
	  = new ConditionalTarget(ctarget.ifFalse, ctarget.ifTrue, language);
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
    return language.getTypeFor(Boolean.TYPE);
  }
}
