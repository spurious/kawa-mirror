package kawa.standard;
import gnu.mapping.Procedure2;

/** Implement the standard Scheme procedure "string>=?". */

public class string_greaterequal_p extends Procedure2
{
  public static boolean apply (Object arg1, Object arg2)
  {
    return arg1.toString().compareTo(arg2.toString()) >= 0;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return apply(arg1, arg2) ? Boolean.TRUE : Boolean.FALSE;
  }
}
