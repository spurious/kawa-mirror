package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "make-rectangular". */

public class make_rectangular extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
   {
     RealNum re = (RealNum) arg1;
     RealNum im = (RealNum) arg2;
     return Complex.make (re, im);
   }
}
