package kawa.lang;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  static public QuoteExp undefined_exp
  = new QuoteExp (Interpreter.undefinedObject);

  public QuoteExp (Object val) { value = val; }
  
  public Object eval (Environment env)
  {
    return value;
  }

  public void compile (Compilation comp, int flags)
  {
    if ((flags & IGNORED) == 0)
      comp.compileConstant (value);
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%quote ");
    SFormat.print (value, ps);
    ps.print(")");
  }
}
