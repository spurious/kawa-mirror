package kawa.standard;
import gnu.math.RealNum;
import gnu.mapping.*;

/** Implement the standard Scheme procedure "positive?". */

public class positive_p extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     if (((RealNum)arg1).sign () > 0)
       return Boolean.TRUE;
     else
       return Boolean.FALSE;
   }
}
