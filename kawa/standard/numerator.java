package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "numerator". */

public class numerator extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     return ((RatNum)arg1).numerator ();
   }
}
