package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

public class logop extends Procedure3
{
  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    int op = IntNum.intValue (arg1);
    return BitOps.bitOp (op, (IntNum) arg2, (IntNum) arg3);
  }
}

