package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "sin". */

public class sin extends Procedure1
{
  public Object apply1 (Object arg1) throws WrongType
  {
    return new DFloNum (Math.sin (((RealNum)arg1).doubleValue ()));
  }
}
