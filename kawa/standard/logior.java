package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

public class logior extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    IntNum result = (IntNum) args[0];
    for (int i = 1; i < len; i++)
      result = BitOps.ior (result, (IntNum) args[i]);
    return result;
   }
}
