package kawa.standard;
import kawa.lang.List;
import gnu.math.IntNum;
import gnu.mapping.Procedure1;

public class length extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return IntNum.make (List.length (arg1));
  }
}
