package kawa.lang;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  public final Object getValue() { return value; }

  static public QuoteExp undefined_exp
  = new QuoteExp (Interpreter.undefinedObject);

  static public QuoteExp falseExp = new QuoteExp(Interpreter.falseObject);

  public QuoteExp (Object val) { value = val; }
  
  public Object eval (Environment env)
  {
    return value;
  }

  public void compile (Compilation comp, Target target)
  {
    comp.compileConstant(value, target);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%quote ");
    SFormat.print (value, ps);
    ps.print(")");
  }
}
