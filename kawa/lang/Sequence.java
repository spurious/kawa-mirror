package kawa.lang;

public abstract class Sequence
{
  /**
   * A safe function to count the length of a sequence.
   * @return the length, or -1 for an infinite list,
   * or -2 for a malformed sequence.
   */
  abstract public int length ();

  abstract public Object elementAt (int index);

  public static Special eofValue = Special.eof;
}
