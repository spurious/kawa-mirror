package gnu.jemacs.lang;
import gnu.mapping.*;

public class ELisp extends kawa.standard.Scheme
{
  public static final kawa.lang.List FALSE = kawa.lang.List.Empty;
  public static final String TRUE = "t";

  /** Get a ELisp symbol for a given (interned) Java string. */
  public static Object getSymbol (String name)
  {
    if (name == "nil")
      return FALSE;
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

  public String getName()
  {
    return "Emacs-Lisp";
  }

  static ELisp instance;

  public static kawa.standard.Scheme getInstance() // Bad return type FIXME
  {
    if (instance == null)
      instance = new ELisp();
    return instance;
  }

  public Environment getNewEnvironment ()
  {
    if (kawa_environment == null)
      initScheme();
    return new ObArray(kawa_environment);
  }
}
