package kawa.standard;
import kawa.lang.*;

import kawa.lang.Procedure1;

public class integer_p extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     return Interpreter.boolObject (arg1 instanceof kawa.math.IntNum);
   }
}
