package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

/** Implement the standard Scheme procedure "exact->inexact". */

public class exact2inexact extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     Numeric num = (Numeric) arg1;
     if (! num.isExact ())
       return num;
     return new DFloNum (((RealNum)num).doubleValue ());
   }
}
