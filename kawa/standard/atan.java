package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

/** Implement the standard Scheme procedure "atan". */

public class atan extends Procedure1or2
{
  public Object apply1 (Object arg1)
  {
    return new DFloNum (Math.atan (((RealNum)arg1).doubleValue ()));
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return new DFloNum (Math.atan2 (((RealNum)arg1).doubleValue (),
				    ((RealNum)arg2).doubleValue ()));
  }
}
