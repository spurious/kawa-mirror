package kawa.lang;
import gnu.bytecode.*;

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

  /**
   * Utility function to test if an Object is true in the Scheme sense.
   * @param object the object to test for truth
   * @return true iff the object is true is the Scheme sense.
   */
  static public final boolean is_true (Object object)
  {
    return object != Interpreter.falseObject;
  }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    if (is_true (test.eval (env)))
      return then_clause.eval (env);
    else if (else_clause != null)
      return else_clause.eval (env);
    else
      return Interpreter.voidObject;
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
    gnu.bytecode.CodeAttr code = comp.getCode();
    Label trueLabel, falseLabel;
    boolean trueInherited, falseInherited;
    // A constant else_clause results from the expansion of (and ...),
    // and also if the else_clause if elided, so we optimize this case.
    if (target instanceof ConditionalTarget
	&& else_clause instanceof QuoteExp)
      {
	falseInherited = true;
	if (is_true(((QuoteExp) else_clause).getValue()))
	  falseLabel = ((ConditionalTarget) target).ifTrue;
	else
	  falseLabel = ((ConditionalTarget) target).ifFalse;
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
    ConditionalTarget ctarget = new ConditionalTarget(trueLabel, falseLabel);
    if (trueInherited)
      ctarget.trueBranchComesFirst = false;
    test.compile(comp, ctarget);
    code.emitIfThen();
    if (! trueInherited)
      {
	trueLabel.define(code);
	then_clause.compileWithPosition(comp, target);
      }
    if (! falseInherited)
      {
	code.emitElse();
	falseLabel.define(code);
	if (else_clause == null)
	  comp.compileConstant(Interpreter.voidObject, target);
	else
	  else_clause.compileWithPosition(comp, target);
      }
    code.emitFi();
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%if ");
    test.print (ps);
    ps.print(" ");
    then_clause.print (ps);
    if (else_clause != null)
      {
	ps.print(" ");
	else_clause.print (ps);
      }
    ps.print(")");
  }

}
