package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "log". */

public class log extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return new DFloNum (Math.log (((RealNum)arg1).doubleValue ()));
  }
}
