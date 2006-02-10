// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/**
 * This class represents a conditional.
 * @author	Per Bothner
 */

public class IfExp extends Expression
{
  Expression test;
  Expression then_clause;
  Expression else_clause;

  public IfExp (Expression i, Expression t, Expression e)
  {
    test = i;  then_clause = t;  else_clause = e;
  }

  protected final Language getLanguage()
  {
    return Language.getDefaultLanguage(); // FIXME
  }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    Language language = getLanguage();
    if (language.isTrue((test.eval(ctx))))
      then_clause.apply(ctx);
    else if (else_clause != null)
      else_clause.apply(ctx);
  }

  public void compile (Compilation comp, Target target)
  {
    compile(test, then_clause,
	    else_clause == null ? QuoteExp.voidExp : else_clause,
	    comp, target);
  }

  public static void compile (Expression test, Expression then_clause,
			      Expression else_clause,
			      Compilation comp, Target target)
  {
    Language language = comp.getLanguage();
    gnu.bytecode.CodeAttr code = comp.getCode();
    Label trueLabel, falseLabel;
    boolean trueInherited, falseInherited;
    // A constant else_clause results from the expansion of (and ...),
    // and also if the else_clause if elided, so we optimize this case.
    if (target instanceof ConditionalTarget
	&& else_clause instanceof QuoteExp)
      {
	falseInherited = true;
	Object value = ((QuoteExp) else_clause).getValue();
	if (language.isTrue(value))
	  falseLabel = ((ConditionalTarget) target).ifTrue;
	else
	  falseLabel = ((ConditionalTarget) target).ifFalse;
      }
    else if (else_clause instanceof ExitExp
             && ((ExitExp) else_clause).result instanceof QuoteExp
             && ((ExitExp) else_clause).block.subTarget instanceof IgnoreTarget)
      {
        falseInherited = true;
        falseLabel = ((ExitExp) else_clause).block.exitLabel;
      }
    else
      {
	falseInherited = false;
	falseLabel = new Label(code);
      }
    // The expansion of "or" creates an IfExp with test==then_clause.
    // In that case, we know that the then_clause must be true.
    // Let's optimize that case.
    if (test == then_clause && target instanceof ConditionalTarget
	&& then_clause instanceof ReferenceExp)
      {
	trueInherited = true;
	trueLabel = ((ConditionalTarget) target).ifTrue;
      }
    else
      {
	trueInherited = false;
	trueLabel = new Label(code); 
      }
    ConditionalTarget ctarget
      = new ConditionalTarget(trueLabel, falseLabel, language);
    if (trueInherited)
      ctarget.trueBranchComesFirst = false;
    test.compile(comp, ctarget);
    code.emitIfThen();
    if (! trueInherited /* && trueLabel.hasFixups()*/)
      {
	trueLabel.define(code);
        Variable callContextSave = comp.callContextVar;
	then_clause.compileWithPosition(comp, target);
        comp.callContextVar = callContextSave;
      }
    if (! falseInherited /* && falseLabel.hasFixups()*/)
      {
	code.emitElse();
	falseLabel.define(code);
        Variable callContextSave = comp.callContextVar;
	if (else_clause == null)
	  comp.compileConstant(Values.empty, target);
	else
	  else_clause.compileWithPosition(comp, target);
        comp.callContextVar = callContextSave;
      }
    else
      code.setUnreachable();
    code.emitFi();
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkIfExp(this);
  }

  protected void walkChildren(ExpWalker walker)
  {
    test = test.walk(walker);
    if (walker.exitValue == null)
      then_clause = walker.walk(then_clause);
    if (walker.exitValue == null && else_clause != null)
     else_clause = walker.walk(else_clause);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(If ", false, ")");
    out.setIndentation(-2, false);
    test.print(out);
    out.writeSpaceLinear();
    then_clause.print (out);
    if (else_clause != null)
      {
	out.writeSpaceLinear();
	else_clause.print (out);
      }
    out.endLogicalBlock(")");
  }
}
