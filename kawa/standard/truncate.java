package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "trunctate". */

public class truncate extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     if (arg1 instanceof IntNum)
       return arg1;
     double d = ((RealNum) arg1).doubleValue ();
     return new DFloNum (d < 0.0 ? Math.ceil (d) : Math.floor (d));
   }
}
