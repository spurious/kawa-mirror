package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class length extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return IntNum.make (List.length (arg1));
  }
}
