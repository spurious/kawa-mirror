package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "make-polar". */

public class make_polar extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
   {
     RealNum re = (RealNum) arg1;
     RealNum im = (RealNum) arg2;
     return Complex.polar (re, im);
   }
}
