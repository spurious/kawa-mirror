package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

import kawa.lang.Procedure1;

public class integer_p extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     boolean result;
     if (arg1 instanceof IntNum)
       result = true;
     else if (arg1 instanceof DFloNum)
       result = Math.IEEEremainder (((DFloNum)arg1).doubleValue(), 1.0) == 0.0;
     else
       result = false;
     return Interpreter.boolObject (result);
   }
}
