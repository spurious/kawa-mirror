package gnu.mapping;
// FIXME will probably repace by some general "attribute" mechanism.

public class Named extends Object {
  protected String sym_name;

  public Named ()
  {
  }

  public Named (String name)
  {
     sym_name = name;
  }

  public final String name()
  {
    return sym_name;
  }

  public String getName()
  {
    return sym_name;
  }

  public final void setName (String name)
  {
    sym_name = name;
  }
}
