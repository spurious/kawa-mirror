package kawa.standard;
import gnu.mapping.Procedure2;

/** Implement the standard Scheme procedure "string-ci=?". */

public class string_ci_equal_p extends Procedure2
{
   public Object apply2 (Object arg1, Object arg2)
   {
     if (arg1.toString().equalsIgnoreCase(arg2.toString()))
       return Boolean.TRUE;
     else
       return Boolean.FALSE;
   }
}
