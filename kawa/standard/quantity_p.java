package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard DSSSL procedure "quantity?". */

public class quantity_p extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     return Interpreter.boolObject (arg1 instanceof Quantity);
   }
}
