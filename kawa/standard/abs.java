package kawa.standard;
import kawa.lang.*;
import gnu.math.Numeric;

public class abs extends Procedure1
{

  public Object apply1 (Object arg1) throws WrongType
  {
    return ((Numeric)arg1).abs ();
  }

}
