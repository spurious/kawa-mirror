package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

/** Implement the standard Scheme procedure "acos". */

public class acos extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return new DFloNum (Math.acos (((RealNum)arg1).doubleValue ()));
  }
}
