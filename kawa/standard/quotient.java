package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "quotient". */

public class quotient extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    return IntNum.quotient (((IntNum)arg1), ((IntNum)arg2));
  }
}
