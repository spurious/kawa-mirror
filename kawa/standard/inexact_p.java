package kawa.standard;
import gnu.math.Numeric;
import gnu.mapping.*;

public class inexact_p extends Procedure1
{
   public Object apply1 (Object arg1)
   {
     boolean result = arg1 instanceof Numeric && ! ((Numeric)arg1).isExact ();
     return result ? Boolean.TRUE : Boolean.FALSE;
   }
}
