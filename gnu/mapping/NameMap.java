package gnu.mapping;

/* Abstract class from mappings to Strings to bindings.
 * This class isn't actually used for anything ... */

public abstract class NameMap extends Procedure1
  implements HasSetter
{
  /** Get the value bound to the given name.
   * @exception gnu.mapping.UnboundLocationException the name has no binding
   * @see Environment#get(Object)
   */
  public final Object getChecked(String name)
  {
    Object value = get(name, Location.UNBOUND);
    if (value == Location.UNBOUND)
      throw new UnboundLocationException(name+" in "+this);
    return value;
  }

  /** Get the value bound to the given name.
   * Returns null if the name has no binding
   * (for compatibility with Java2 Collections framework).
   * @see Environment#getChecked(String)
   */
  public Object get (Object name)
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
