package kawa.lang;
import gnu.expr.Special;

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

  public Object[] toArray() 
  { 
    return toArray(null); 
  } 

  public Object[] toArray(Object[] arr) 
  { 
    int alen = arr == null ? -1 : arr.length; 
    int len = length(); 
    if (len > alen) 
    { 
      arr = new Object[len]; 
      alen = len; 
    } 
    for (int i = 0;  i < len;  i++) 
    { 
      arr[i] = elementAt(i); 
    } 
    if (len < alen) 
      arr[len] = null; 
    return arr;
  }
}
