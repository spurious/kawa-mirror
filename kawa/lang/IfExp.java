package kawa.lang;
import codegen.*;

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
      return Interpreter.undefinedObject;
  }

  public void compile (Compilation comp, int flags)
  {
    test.compile (comp, 0);
    comp.compileConstant (Interpreter.falseObject);
    Label else_label = new Label (comp.method);
    comp.method.compile_goto_ifeq (else_label);
    then_clause.compile (comp, flags);
    Label end_label;
    if (else_clause == null && (flags & IGNORED) != 0)
      end_label = null;
    else
      {
	end_label = new Label (comp.method);
	if (comp.method.reachableHere ())
	  comp.method.compile_goto (end_label);
      }

    else_label.define (comp.method);
    if (else_clause != null)
      else_clause.compile (comp, flags);
    else if ((flags & IGNORED) == 0)
      comp.compileConstant (Interpreter.undefinedObject);
    if (end_label != null)
      end_label.define (comp.method);
  }

  public void print (java.io.PrintStream ps)
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
