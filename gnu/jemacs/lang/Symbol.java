package gnu.jemacs.lang;
import gnu.mapping.*;

public class Symbol extends Binding
{
  Object functionValue;

  public Symbol (String name)
  {
    super(name);
  }

  public static boolean isSymbol(Object val)
  {
    return val instanceof String || val == ELisp.FALSE;
  }

  public static boolean isBound(Object sym)
  {
    if (sym == ELisp.FALSE)
      return true;
    Binding binding = Environment.getCurrent().lookup((String) sym);
    return binding != null && binding.isBound();
  }
  
  public static Binding getBinding(Environment env, Object sym)
  {
    if (sym == ELisp.FALSE)
      sym = "nil";
    return env.getBinding((String) sym);
  }

  public static Binding getBinding(Object sym)
  {
    if (sym == ELisp.FALSE)
      sym = "nil";
    return Environment.getCurrentBinding((String) sym);
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
}
