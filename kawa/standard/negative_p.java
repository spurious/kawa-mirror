package kawa.standard;
import kawa.lang.*;
import kawa.math.RealNum;

/** Implement the standard Scheme procedure "negative?". */

public class negative_p extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     if (((RealNum)arg1).sign () == -1)
       return Interpreter.trueObject;
     else
       return Interpreter.falseObject;
   }
}
