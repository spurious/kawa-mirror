package kawa.standard;
import gnu.math.*;
import gnu.mapping.Procedure2;

public class logtest extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    return BitOps.test((IntNum) arg1, (IntNum) arg2) ? Boolean.TRUE
      : Boolean.FALSE;
  }
}
