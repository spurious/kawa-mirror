package kawa.standard;
import gnu.math.IntNum;
import gnu.mapping.*;

public class ashift extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    return IntNum.shift ((IntNum) arg1, IntNum.intValue (arg2));
  }
}
