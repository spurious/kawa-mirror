package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

public class logand extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.minusOne();
    IntNum result = (IntNum) args[0];
    for (int i = 1; i < len; i++)
      result = BitOps.and (result, (IntNum) args[i]);
    return result;
   }
}
