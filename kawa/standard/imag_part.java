package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "imag-part". */

public class imag_part extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     Complex c = (Complex) arg1;
     return Quantity.make (c.im(), c.unit());
   }
}
