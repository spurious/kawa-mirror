package gnu.mapping;

/* Abstract class from mappings to Strings to bindings. */

public abstract class NameMap extends Procedure1 implements HasSetter
  // implements java.util.Map
{
  public abstract Object getChecked (String key);

  public abstract Object put (String key, Object value);

  /** Get the value bound to the given name.
   * Returns null if the name has no binding
   * (for compatibility with Java2 Collections framework).
   * @see Environment#getChecked(String)
   */
  public final Object get (Object name)
  {
    try
      {
	return getChecked((String) name);
      }
    catch (UnboundSymbol ex)
      {
	return null;
      }
  }

  public Object apply1 (Object arg)
  {
    return get ((String) arg);
  }

  public void set1 (Object arg, Object value)
  {
    put ((String) arg, value);
  }

}
