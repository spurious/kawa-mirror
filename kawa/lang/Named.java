package kawa.lang;

public class Named extends Object {
  public String sym_name;

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

  public final String getName()
  {
    return sym_name;
  }

  public final void setName (String name)
  {
    sym_name = name;
  }
}
