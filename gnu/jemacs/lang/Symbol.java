package gnu.jemacs.lang;
import gnu.mapping.*;

/** Support for ELisp Symbols.
 * For now there are actually no "Symbol" objects.
 * Instead, at the ELisp level, the progrmmer uses java.lang.String objects.
 * This is for compatibility with Scheme, better integration with Java,
 * and because Strings are less "heighy-weight".  The special symbol `nil'
 * is actually the value gnu.kawa.util.LList.Empty.
 * We use an Environment (ObArray) to map from from String to Binding2
 * objects (perhaps in the future we will use Symbol instances instead of
 * Binding2 instances).  The mapping from identifies to Binding2 objects
 * is does at class-initialization time, so no hash-table search is
 * needed for normal function or variable name lookup. */

public class Symbol extends Binding2
{
  public Symbol (String name)
  {
    super(name);
  }

  public static boolean isSymbol(Object val)
  {
    return val instanceof String || val == ELisp.FALSE
      || val instanceof Binding2;
  }

  public static boolean isBound(Object sym)
  {
    if (sym == ELisp.FALSE)
      return true;
    Binding binding = Environment.getCurrent().lookup((String) sym);
    return binding != null && binding.isBound();
  }
  
  public static Binding2 getBinding(Environment env, Object sym)
  {
    if (sym == ELisp.FALSE)
      sym = "nil";
    return Binding2.getBinding2(env, (String) sym);
  }

  public static Binding2 getBinding(Object sym)
  {
    if (sym == ELisp.FALSE)
      sym = "nil";
    return Binding2.getBinding2(Environment.getCurrent(), (String) sym);
  }

  public static void setValueBinding(Object symbol, Object value)
  {
    getBinding(symbol).set(value);
  }

  public static Object getPrintName(String sym)
  {
    return ELisp.getString(sym);
  }

  public static Object getPrintName(Object sym)
  {
    return sym == ELisp.FALSE ? "nil" : ELisp.getString((String) sym);
  }

  public static Object getFunctionBinding (Object symbol)
  {
    Binding2 binding = getBinding(symbol);
    return binding.functionValue;
  }

  public static Object getFunctionBinding (Environment environ, Object symbol)
  {
    Binding2 binding = getBinding(environ, symbol);
    return binding.functionValue;
  }

  public static void setFunctionBinding (Environment environ,
					 Object symbol, Object value)
  {
    Binding2 binding = getBinding(environ, symbol);
    System.err.println("setFun "+symbol+" bind:"+binding+" val:"+value);
    binding.functionValue = value;
  }
}
