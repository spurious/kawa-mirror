package kawa.lang;

public class Named extends Object implements Nameable {
  public Symbol sym_name;

  public Named ()
  {
  }

  public Named (String name)
  {
     sym_name = Symbol.make (name);
  }

  public Named (Symbol name)
  {
     sym_name = name;
  }

  public final Symbol name()
  {
    return sym_name;
  }

  public final void setName (Symbol name)
  {
    sym_name = name;
  }
}
