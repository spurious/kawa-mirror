package kawa.standard;
import gnu.math.IntNum;
import gnu.math.BitOps;
import gnu.mapping.Procedure1;

/** Implements the Common Lisp function "logcount". */

public class logcount extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return IntNum.make (BitOps.bitCount ((IntNum)arg1));
  }
}
