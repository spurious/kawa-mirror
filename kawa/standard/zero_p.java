package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "zero?". */

public class zero_p extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     if (((Numeric)arg1).isZero ())
       return Interpreter.trueObject;
     else
       return Interpreter.falseObject;
   }
}
