package kawa.standard;
import gnu.math.IntNum;
import gnu.mapping.*;
import gnu.kawa.util.*;

public class list_tail extends Procedure2
{
  static public Object listTail (Object list, int count)
  {
    while (--count >= 0)
      {
	if (! (list instanceof Pair))
	  throw new IndexOutOfBoundsException("List is too short.");
	Pair pair = (Pair) list;
	list = pair.cdr;
      }
    return list;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    Pair list = (Pair)arg1;
    int count = IntNum.intValue (arg2);
    return listTail (list, count);
  }
}
