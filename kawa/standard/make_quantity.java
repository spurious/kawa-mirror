package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

public class make_quantity extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
   {
     Unit unit;
     if (arg2 instanceof Unit)
       unit = (Unit) arg2;
     else
       {
	 String name = arg2.toString();
	 unit = Unit.lookup (name);
	 if (unit == null)
	   throw new GenericError ("unknown unit: "+name);
       }
     return Quantity.make ((Complex) arg1, unit);
   }
  
}
