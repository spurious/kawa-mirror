package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class odd_p extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     return Interpreter.boolObject (((IntNum)arg1).isOdd ());
   }
}
