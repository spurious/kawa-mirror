package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "round". */

public class round extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     if (arg1 instanceof IntNum)
       return arg1;
     double d = ((RealNum) arg1).doubleValue ();
     return new DFloNum (Math.floor (d + 0.5));
   }
}
