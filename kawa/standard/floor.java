package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "floor". */

public class floor extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     return ((RealNum) arg1).toInt (Numeric.FLOOR);
   }
}
