package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

public class setter extends Procedure1
{
  public static setter setterProcedure = new setter();

  public Object apply1 (Object arg)
  {
    return ((Procedure)arg).getSetter();
  }
}
