package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "modulo". */

public class modulo extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    return IntNum.modulo (((IntNum)arg1), ((IntNum)arg2));
  }
}
