package kawa.standard;
import kawa.lang.*;

public class setter extends Procedure1
{
  public static setter setterProcedure = new setter();

  public setter()
  {
    super("setter");
  }

  public Object apply1 (Object arg)
  {
    return ((Procedure)arg).getSetter();
  }
}
