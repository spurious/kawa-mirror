package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "expt". */

public class expt extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg1 instanceof RatNum && arg2 instanceof IntNum)
      return RatNum.power ((RatNum) arg1, (IntNum) arg2);
    return new DFloNum (Math.pow (((RealNum)arg1).doubleValue (),
				  ((RealNum)arg2).doubleValue ()));
  }
}
