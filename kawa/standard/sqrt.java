package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "sqrt". */

public class sqrt extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return new DFloNum (Math.sqrt (((RealNum)arg1).doubleValue ()));
  }
}
