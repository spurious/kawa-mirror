package gnu.mapping;

/* Abstract class from mappings to Strings to bindings. */

public abstract class NameMap extends Procedure1 implements HasSetter
  // implements java.util.Map
{
  /** Get the value bound to the given name.
   * @exception gnu.mapping.UnboundSymbol the name has no binding
   * @see Environment#get(Object)
   */
  public final Object getChecked(String name)
  {
    Object value = get(name, Symbol.UNBOUND);
    if (value == Symbol.UNBOUND)
      throw new UnboundSymbol(name);
    return value;
  }

  /** Get the value bound to the given name.
   * Returns null if the name has no binding
   * (for compatibility with Java2 Collections framework).
   * @see Environment#getChecked(String)
   */
  public final Object get (Object name)
  {
    return get((String) name, null);
  }

  public abstract Object get (String key, Object defaultValue);

  public abstract Object put (String key, Object value);

  public Object apply1 (Object arg)
  {
    return get ((String) arg);
  }

  public void set1 (Object arg, Object value)
  {
    put ((String) arg, value);
  }

}
