package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

public class logand extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
