package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

/** Implements the extended DSSSL procedure "quantity->unit". */

public class quantity2unit extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     return ((Quantity) arg1).unit();
   }
}
