package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "number?". */

public class number_p extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     return Interpreter.boolObject (arg1 instanceof Numeric);
   }
}
