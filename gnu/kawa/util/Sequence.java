package gnu.kawa.util;
import gnu.expr.Special;

public abstract class Sequence // FUTURE: implements java.util.List
{
  /**
   * A safe function to count the length of a sequence.
   * @return the length, or -1 for an infinite list,
   * or -2 for a malformed sequence.
   */
  abstract public int length ();

  abstract public Object get (int index);

  public final Object elementAt (int index) { return get(index); }

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
      Class componentType = arr.getClass().getComponentType();
      arr = (Object[]) java.lang.reflect.Array.newInstance(componentType, len);
      alen = len; 
    }
    
    java.util.Enumeration e = elements();
    for (int i = 0;  e.hasMoreElements(); i++)
    {
      arr[i] = e.nextElement();
    } 
    if (len < alen) 
      arr[len] = null; 
    return arr;
  }

  public java.util.Enumeration elements()
  {
    return new SeqEnumeration(this);
  }
}

class SeqEnumeration implements java.util.Enumeration
{
  Sequence seq;
  int length;
  int current;

  public SeqEnumeration(Sequence seq)
  {
    this.seq = seq;
    this.length = seq.length();
    this.current = 0;
  }

  public boolean hasMoreElements()
  {
    return current < length;
  }

  public Object nextElement()
  {
    if (current < length)
      return seq.get(current++);
    throw new java.util.NoSuchElementException();
  }
}
