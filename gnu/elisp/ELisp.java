package gnu.elisp;
import gnu.mapping.*;

public class ELisp extends kawa.standard.Scheme
{
  /** Get a ELisp symbol for a given (interned) Java string. */
  public static Object getSymbol (String name)
  {
    if (name == "nil")
      return kawa.lang.List.Empty;
    // return Environment.getCurrentBinding(name);
    return name;
  }

  /** Get a ELisp string for a given Java string. */
  public static Object getString (String name)
  {
    return new kawa.lang.FString(name);
  }

  /** Get a ELisp string for a given ELisp symbol. */
  public static Object getString (Binding symbol)
  {
    return getString(symbol.getName());
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new ELispReader(inp, messages);
  }

  static ELisp instance;

  public static kawa.standard.Scheme getInstance() // Bad return type FIXME
  {
    if (instance == null)
      instance = new ELisp();
    return instance;
  }
}
