package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.lists.*;

/** Support for Lisp Symbols.
 * The special symbol `nil' is actually the value gnu.lists.LList.Empty. */

public class Symbols
{
  private Symbols ()
  {
  }

  public static boolean isSymbol(Object val)
  {
    return val instanceof String || val == Lisp2.FALSE
      || val instanceof Symbol;
  }

  public static boolean isBound(Object sym)
  {
    if (sym == Lisp2.FALSE)
      return true;
    Environment env = Environment.getCurrent();
    Symbol symbol;
    if (sym instanceof Symbol)
      symbol = (Symbol) sym;
    else
      symbol = env.defaultNamespace().lookup((String) sym);
    return symbol != null && env.isBound(symbol);
  }
  
  public static Symbol getSymbol(Environment env, Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return sym instanceof Symbol ? (Symbol) sym
      : env.defaultNamespace().getSymbol((String) sym);
  }

  public static Symbol getSymbol(Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return sym instanceof Symbol ? (Symbol) sym
      : Namespace.getDefaultSymbol((String) sym); // FIXME
  }

  public static Object getPrintName(Object sym)
  {
    return sym == Lisp2.FALSE ? "nil"
      : Lisp2.getString(((Symbol) sym).getName());
  }

  public static Object getFunctionBinding (Object symbol)
  {
    return Environment.getCurrent().getFunction(getSymbol(symbol));
  }

  public static Object getFunctionBinding (Environment environ, Object symbol)
  {
    return environ.getFunction(getSymbol(symbol));
  }

  public static void setFunctionBinding (Environment environ,
					 Object symbol, Object newValue)
  {
    environ.put(getSymbol(symbol), EnvironmentKey.FUNCTION, newValue);
  }

}
