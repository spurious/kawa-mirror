package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

public class logior extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
