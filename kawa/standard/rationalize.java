package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "rationalize". */

public class rationalize extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    RealNum x = (RealNum) arg1;
    RealNum y = (RealNum) arg2;
    return RatNum.rationalize ((RealNum) x.sub(y), (RealNum) x.add(y));
  }
}


