package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

/** Implement the standard Scheme procedure "modulo". */

public class modulo extends Procedure2
{
  // Should generalize to arbitrary reals:  [FIXME]
  //   x mod y = x - y * floor(x/y), for y /= 0
  //   x mod 0 = x    

  public Object apply2 (Object arg1, Object arg2)
  {
    return IntNum.modulo (((IntNum)arg1), ((IntNum)arg2));
  }
}
