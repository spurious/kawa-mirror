package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

/** Implement the standard Scheme procedure "asin". */

public class asin extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return new DFloNum (Math.asin (((RealNum)arg1).doubleValue ()));
  }
}
