package kawa.standard;
import kawa.lang.*;
import gnu.math.Numeric;

public class inexact_p extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     boolean result = arg1 instanceof Numeric && ! ((Numeric)arg1).isExact ();
     return Interpreter.boolObject (result);
   }
}
