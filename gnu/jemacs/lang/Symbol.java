package gnu.jemacs.lang;
import gnu.mapping.*;

public class Symbol
{
  public static boolean isSymbol(Object val)
  {
    return val instanceof String || val == ELisp.FALSE;
  }

  public static boolean isBound(Object sym)
  {
    // Inefficient - may create useless Binding.  FIXME.
    Binding binding = getBinding(sym);
    return binding.isBound();
  }

  public static Binding getBinding(Object sym)
  {
    if (sym == ELisp.FALSE)
      sym = "nil";
    return Environment.getCurrentBinding((String) sym);
  }

  public static Object getPrintName(String sym)
  {
    return ELisp.getString(sym);
  }
}
