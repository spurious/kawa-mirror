package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;
import gnu.math.BitOps;

public class bit_extract extends Procedure3
{
  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    return BitOps.extract ((IntNum) arg1,
			   IntNum.intValue (arg2), IntNum.intValue (arg3));
  }
}
