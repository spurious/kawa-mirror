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
    return ((List) arg1).toVector ();
  }
}
