package kawa.standard;

import kawa.lang.Procedure1;

public class length extends kawa.lang.Procedure1
{
  public kawa.standard.length()
  {
    super("length");
  }

  /** Count the length of a list.
   * Note: does not catch circular lists!
   * @param arg the list to count
   * @return the length
   */
  static public final int length (Object arg)
  {
    int count = 0;
    for ( ; arg instanceof kawa.lang.pair; arg = ((kawa.lang.pair)arg).cdr)
      count++;
    return count;
  }

  public Object apply1 (Object arg1)
  {
    return new java.lang.Integer (length (arg1));
  }
}
