package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

public class substring extends Procedure3
{
  public static StringBuffer substring (StringBuffer str, int start, int end)
  {
    return new StringBuffer (str.toString().substring (start, end));
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongType, GenericError
  {
    return substring ((StringBuffer)arg1,
		      IntNum.intValue (arg2), IntNum.intValue (arg3));
  }
}
