package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

/** Implements the extended DSSSL procedure "quantity->unit". */

public class quantity2unit extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     Quantity q = (Quantity) arg1;
     return q.unit();
   }
}
