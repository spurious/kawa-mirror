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

  public void compile (Compilation comp, int flags)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    test.compile (comp, 0);
    comp.compileConstant (Interpreter.falseObject);
    code.emitIfNEq();
    then_clause.compile_with_linenumber (comp, flags);
    if (else_clause != null || (flags & IGNORED) == 0)
      {
	code.emitElse();
	if (else_clause != null)
	  else_clause.compile_with_linenumber (comp, flags);
	else
	  comp.compileConstant (Interpreter.voidObject);
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
