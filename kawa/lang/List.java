package kawa.lang;
import java.io.PrintStream;

/**
 * Semi-abstract class for Scheme lists.
 * Includes singleton static Empty, and the Pair sub-class.
 * @author	Per Bothner
 */

public class List extends Sequence implements Printable
{
  protected List () { }

  static public final List Empty = new List ();

  public void print(java.io.PrintStream ps)
  {
    // Gets overridden for Pair.
    ps.print("()");
  }

  public int length ()
  {
    return 0;
  }

  public Object elementAt (int index)
  {
    throw new ArrayIndexOutOfBoundsException (index);
  }
  
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

  /**
   * A safe function to count the length of a list.
   * @param obj the putative list to measure
   * @return the length, or -1 for a circular list, or -2 for an improper list
   */
  static public final int list_length (Object obj)
  {
    // Based on list-length implementation in
    // Guy L Steele jr: "Common Lisp:  The Language", 2nd edition, page 414
    int n = 0;
    Object slow = obj;
    Object fast = obj;
    for (;;)
      {
	if (fast == Empty)
	  return n;
	if (! (fast instanceof Pair))
	  return -2;
	Pair fast_pair = (Pair) fast;
	if (fast_pair.cdr == Empty)
	  return n+1;
	if (fast == slow && n > 0)
	  return -1;
	if (! (fast_pair.cdr instanceof Pair) || !(slow instanceof Pair))
	  return -2;
	slow = ((Pair)slow).cdr;
	fast = ((Pair)fast_pair.cdr).cdr;
	n += 2;
      }
  }

  public static List makeList (Object[] vals, int offset, int length)
  {
    List result = Interpreter.nullObject;
    for (int i = length;  --i >= 0; )
      result = new Pair (vals[offset+i], result);
    return result;
  }

  public static List makeList (Object[] vals, int offset)
  {
    List result = Interpreter.nullObject;
    for (int i = vals.length - offset;  --i >= 0; )
      result = new Pair (vals[offset+i], result);
    return result;
  }

  public kawa.lang.Vector toVector ()
  {
    int len = length();

    Object[] values = new Object[len];
    Object list = this;
    for (int i=0; i < len; i++)
      {
	Pair pair = (Pair) list;
	values[i] = pair.car;
	list = pair.cdr;
      }
    return new kawa.lang.Vector (values);
  }
}
