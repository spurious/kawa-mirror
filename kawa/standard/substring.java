package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

public class substring extends Procedure3
{
  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongType, GenericError
  {
    return ((FString)arg1).copy (IntNum.intValue (arg2),
				      IntNum.intValue (arg3));
  }
}
