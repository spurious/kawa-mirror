package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;
import gnu.kawa.functions.Arithmetic;

/** Implement the standard Scheme procedure "expt". */

public class expt extends Procedure2
{
  public static final expt expt = new expt("expt");

  public expt (String name)
  {
    super(name);
  }

  public static IntNum expt (IntNum x, int y)
  {
    return IntNum.power(x, y);
  }

  public static Numeric expt (Object arg1, Object arg2)
  {
    arg1 = Promise.force(arg1);
    arg2 = Promise.force(arg2);
    if (arg2 instanceof IntNum)
      return Arithmetic.asNumeric(arg1).power((IntNum) arg2);
    return Complex.power ((Complex) arg1, (Complex) arg2);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return expt(arg1, arg2);
  }
}
