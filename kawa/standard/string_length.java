package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class string_length extends Procedure1
{
  public Object apply1 (Object arg1)
       throws kawa.lang.WrongType
  {
    return IntNum.make (((FString)arg1).length());
  }
}
