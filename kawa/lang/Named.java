package kawa.lang;

public class Named extends Object implements Nameable {
  public String name;

  public Named (String name)
  {
     this.name = name;
  }

  public String name()
  {
    return name;
  }
}
