package kawa.standard;
import gnu.math.*;
import gnu.mapping.Procedure2;

public class logbit_p extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    boolean result = BitOps.bitValue ((IntNum) arg1, IntNum.intValue (arg2));
    return result ? Boolean.TRUE : Boolean.FALSE;
  }
}
