package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "acos". */

public class acos extends Procedure1
{
  public Object apply1 (Object arg1) throws WrongType
  {
    return new DFloNum (Math.acos (((RealNum)arg1).doubleValue ()));
  }
}
