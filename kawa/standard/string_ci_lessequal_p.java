package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure2;

/** Implement the standard Scheme procedure "string-ci<=?". */

public class string_ci_lessequal_p extends Procedure2
{
  public static boolean apply (Object arg1, Object arg2)
  {
    String s1 = arg1.toString().toLowerCase();
    String s2 = arg2.toString().toLowerCase();
    return s1.compareTo(s2) <= 0;
   }

  public Object apply2 (Object arg1, Object arg2)
  {
    return apply(arg1, arg2) ? Boolean.TRUE : Boolean.FALSE;
  }
}
