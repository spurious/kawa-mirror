package gnu.expr;
import gnu.mapping.*;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  public final Object getValue() { return value; }

  public final gnu.bytecode.Type getType()
  {
    if (value == Values.empty)
      return gnu.bytecode.Type.void_type;
    if (value == null)
      return gnu.bytecode.Type.nullType;
    return gnu.bytecode.Type.make(value.getClass());
  }

  static public QuoteExp undefined_exp
  = new QuoteExp (Undefined.getInstance());
  static public QuoteExp voidExp = new QuoteExp (Values.empty);
  static public QuoteExp trueExp = new QuoteExp(Boolean.TRUE);
  static public QuoteExp falseExp = new QuoteExp(Boolean.FALSE);
  static public QuoteExp nullExp = new QuoteExp(null);

  public QuoteExp (Object val) { value = val; }
  
  public Object eval (Environment env)
  {
    return value;
  }

  public void compile (Compilation comp, Target target)
  {
    comp.compileConstant(value, target);
  }
 
  protected Expression walk (ExpWalker walker)
  {
    return walker.walkQuoteExp(this);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%quote ");
    SFormat.print (value, ps);
    ps.print(")");
  }
}
