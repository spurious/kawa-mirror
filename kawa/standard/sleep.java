package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

/** Implements the extended procedure "sleep". */

public class sleep extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     Quantity q = (Quantity) arg1;
     Unit u = q.unit();
     double seconds;
     // If q is either dimensionless or its unit is a multiple of Unit.second:
     if (u == Unit.Empty
	 || u.dimensions() == Unit.second.dimensions())
       seconds = q.doubleValue();
     else
       throw new GenericError("bad unit for sleep");
     long millis = (long) (seconds * 1000.0);
     int nanos = (int) (seconds * 1e9 - millis * 1e6);
     try
       {
	 Thread.sleep (millis, nanos);
       }
     catch (InterruptedException ex)
       {
	 throw new GenericError("sleep was interrupted");
       }
     return Values.empty;
   }
  
}
