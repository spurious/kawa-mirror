package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "exp". */

public class exp extends Procedure1
{
  public Object apply1 (Object arg1) throws WrongType
  {
    return new DFloNum (Math.exp (((RealNum)arg1).doubleValue ()));
  }
}
