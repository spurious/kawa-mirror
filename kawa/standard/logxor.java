package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

public class logxor extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    IntNum result = (IntNum) args[0];
    for (int i = 1; i < len; i++)
      result = BitOps.xor (result, (IntNum) args[i]);
    return result;
   }
}
