package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;
import kawa.math.BitOps;

/** Implements the Common Lisp function "logcount". */

public class logcount extends Procedure1
{
  public Object apply1 (Object arg1) throws WrongType
  {
    return IntNum.make (BitOps.bitCount ((IntNum)arg1));
  }
}
