package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class length extends Procedure1
{
  /** Count the length of a list.
   * Note: does not catch circular lists!
   * @param arg the list to count
   * @return the length
   */
  static public final int length (Object arg)
  {
    int count = 0;
    for ( ; arg instanceof Pair; arg = ((Pair)arg).cdr)
      count++;
    return count;
  }

  public Object apply1 (Object arg1)
  {
    return IntNum.make (length (arg1));
  }
}
