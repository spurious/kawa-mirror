package kawa.standard;
import kawa.lang.*;
import kawa.math.RealNum;

/** Implement the standard Scheme procedure "positive?". */

public class positive_p extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     if (((RealNum)arg1).sign () > 0)
       return Interpreter.trueObject;
     else
       return Interpreter.falseObject;
   }
}
