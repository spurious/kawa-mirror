package gnu.elisp;
import gnu.mapping.*;

public class ELisp
{
  /** Get a ELisp symbol for a given (interned) Java string. */
  public static Binding getSymbol (String name)
  {
    return Environment.getCurrentBinding(name);
  }

  /** Get a ELisp string for a given Java string. */
  public static Object getString (String name)
  {
    return name;  // FIXME!
  }

  /** Get a ELisp string for a given ELisp symbol. */
  public static Object getString (Binding symbol)
  {
    return getString(symbol.getName());
  }
}
