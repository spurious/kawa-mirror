package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class ashift extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError
  {
    return IntNum.shift ((IntNum) arg1, IntNum.intValue (arg2));
  }
}
