package gnu.mapping;

/* Abstract class from mappings to Strings to bindings. */

public abstract class NameMap extends Procedure1 implements HasSetter
  // implements java.util.Map
{
  public abstract Object get (String key);

  public abstract Object put (String key, Object value);

  public Object apply1 (Object arg)
  {
    return get ((String) arg);
  }

  public void set1 (Object value, Object arg)
  {
    put ((String) arg, value);
  }

}
