package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "sqrt". */

public class sqrt extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    Quantity q = (Quantity) arg1;
    Complex z = q.number().sqrt();
    Unit u = q.unit();
    if (u == Unit.Empty)
      return z;
    return Complex.make (z, u.sqrt());
  }
}
