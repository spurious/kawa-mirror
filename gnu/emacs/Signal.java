package gnu.emacs;

public class Signal extends RuntimeException
{
  String name;
  Object data;

  public Signal(String name, Object data)
  {
    this.name = name;
    this.data = data;
  }

  public static void signal(String name, Object data)
  {
    throw new Signal(name, data);
  }

  public static void signal(String name)
  {
    throw new Signal(name, null);
  }

  public static void error(Object data)
  {
    throw new Signal("error", data);
  }
}
