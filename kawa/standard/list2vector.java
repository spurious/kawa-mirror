package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "list->vector".
 */

public class list2vector extends Procedure1
{
  public list2vector()
  {
    super("list->vector");
  }

  public Object apply1 (Object arg1)
       throws WrongType
  {
    int len = ((List) arg1).length();

    Object[] values = new Object[len];

    for (int i=0; i < len; i++)
      {
	Pair pair = (Pair) arg1;
	values[i] = pair.car;
	arg1 = pair.cdr;
      }
    return new kawa.lang.Vector (values);
  }
}
