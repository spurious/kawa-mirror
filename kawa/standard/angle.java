package kawa.standard;
import kawa.lang.*;
import gnu.math.Quantity;

/** Implement the standard Scheme procedure "angle". */

public class angle extends Procedure1
{

  public Object apply1 (Object arg1) throws WrongType
  {
    return ((Quantity)arg1).number().angle();
  }

}
