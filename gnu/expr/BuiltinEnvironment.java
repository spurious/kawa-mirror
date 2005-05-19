package gnu.expr;
import gnu.mapping.*;

/** An Environment containing the default bindings for the current Language.
 * All <code>lookup</code> operatiosn are indirected to the
 * current <code>Language</code>. */

public class BuiltinEnvironment extends Environment
{
  static final BuiltinEnvironment instance = new BuiltinEnvironment();
  static { instance.setName("language-builtins"); }

  public static BuiltinEnvironment getInstance() { return instance; }

  public Environment getLangEnvironment ()
  {
    Language lang = Language.getDefaultLanguage();
    return lang == null ? null : lang.getLangEnvironment();
  }

  public NamedLocation lookup (Symbol name, Object property, int hash)
  {
    if (property == ThreadLocation.ANONYMOUS)
      return null;
    Language lang = Language.getDefaultLanguage();
    return lang == null ? null : lang.lookupBuiltin(name, property, hash);
  }

  public NamedLocation getLocation (Symbol key, Object property, boolean create)
  {
    throw new RuntimeException();
  }

  public void define (Symbol key, Object property, Object newValue)
  {
    throw new RuntimeException();
  }

  public LocationEnumeration enumerateLocations ()
  {
    return getLangEnvironment().enumerateLocations();
  }

  public LocationEnumeration enumerateAllLocations ()
  {
    return getLangEnvironment().enumerateLocations();
  }

  protected boolean hasMoreElements (LocationEnumeration it)
  {
    throw new RuntimeException();
  }

  public NamedLocation addLocation (Symbol name, Object prop, Location loc)
  {
    throw new RuntimeException();
  }
}
