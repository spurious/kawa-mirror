package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "remainder". */

public class remainder extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    return IntNum.remainder (((IntNum)arg1), ((IntNum)arg2));
  }
}
