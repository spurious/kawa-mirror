package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "inexact->exact". */

public class inexact2exact extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     Numeric num = (Numeric) arg1;
     if (num.isExact ())
       return num;
     return IntNum.make ((long) (((RealNum)num).doubleValue ()));
   }
}
