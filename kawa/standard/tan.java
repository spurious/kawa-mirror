package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "tan". */

public class tan extends Procedure1
{
  public Object apply1 (Object arg1) throws WrongType
  {
    return new DFloNum (Math.tan (((RealNum)arg1).doubleValue ()));
  }
}
