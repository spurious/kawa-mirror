package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

/** Implements the standard DSSSL procedure "quantity->number". */

public class quantity2number extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     Quantity q = (Quantity) arg1;
     Unit u = q.unit();
     double factor = u.doubleValue();
     if (factor == 1.0)
       return q.number();
     else
       return Complex.make (q.reValue(), q.imValue());
   }
  
}
