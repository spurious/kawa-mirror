package kawa.lang;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  public QuoteExp (Object val) { value = val; }
  
  public Object eval (Environment env)
  {
    return value;
  }

  public void compile (Compilation comp, boolean ignore_result)
  {
    if (!ignore_result)
      comp.compileConstant (value);
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%quote ");
    kawa.lang.print.print (value, ps);
    ps.print(")");
  }
}
