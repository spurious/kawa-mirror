package kawa.lang;

/**
 * This class represents a variable reference (an identifier).
 * @author	Per Bothner
 */

public class ReferenceExp extends Expression
{
  Symbol symbol;
  Declaration binding;
  public String string_name () { return symbol.toString (); }

  public ReferenceExp (Symbol symbol) { this.symbol = symbol; }

  public Object eval (Environment env)
       throws kawa.lang.UnboundSymbol
  {
    if (binding != null && env.values != null)
      return env.values[binding.index];
    Object val = env.interp.lookup (symbol);
    if (val == null)
      throw new UnboundSymbol(symbol.toString ());
    return val;
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%ref ");
    kawa.lang.print.print (symbol, ps);
    ps.print(")");
  }
}
