package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "ceiling". */

public class ceiling extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     return ((RealNum) arg1).toInt (Numeric.CEILING);
   }
}
