package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "floor". */

public class floor extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     RealNum num = (RealNum) arg1;
     return new DFloNum (Math.floor (num.doubleValue ()));
   }
}
