package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "expt". */

public class expt extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg2 instanceof IntNum)
      return ((Numeric) arg1).power((IntNum) arg2);
    return Complex.power ((Complex) arg1, (Complex) arg2);
  }
}
