package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "inexact->exact". */

public class inexact2exact extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     if (arg1 instanceof RealNum)
       return ((RealNum)arg1).toExact ();
     return arg1;
   }
}
