package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "real-part". */

public class real_part extends 
Procedure1
{
  public Object apply1 (Object arg1)
   {
     return ((RatNum)arg1).numerator ();
   }
}
