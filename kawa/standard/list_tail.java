package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;

public class list_tail extends Procedure2
{
  static public Object listTail (Object list, int count)
       throws GenericError
  {
    while (--count >= 0)
      {
	if (! (list instanceof Pair))
	  throw new GenericError("List is too short.");
	Pair pair = (Pair) list;
	list = pair.cdr;
      }
    return list;
  }

  public Object apply2 (Object arg1, Object arg2)
       throws GenericError
  {
    Pair list = (Pair)arg1;
    int count = IntNum.intValue (arg2);
    return listTail (list, count);
  }
}
