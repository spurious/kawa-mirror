package gnu.kawa.util;
import gnu.mapping.Printable;
import gnu.mapping.SFormat;

public abstract class UniformVector extends Sequence implements Printable
{
  public abstract void setElementAt (Object new_value, int index);

  public abstract String getTag();

  /*
  public long longValue(int i)
  {
    return (long) intValue(i);
  }
  */

  public final Object set(int index, Object value)
  {
    Object old = get(index);
    setElementAt(value, index);
    return old;
  }

  /** Replace all the elements with a new value. */ 
  public void setAll (Object value)
  {
     for (int index = length(); --index >= 0; )
       setElementAt(value, index);
  }

  public void copy(Sequence src)
  {
    java.util.Enumeration e = src.elements();
    for (int i = 0;  e.hasMoreElements(); i++)
      set(i, e.nextElement());
  }

  public void print(int index, java.io.PrintWriter ps)
  {
    SFormat.print(get(index), ps);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print('#');
    ps.print(getTag());
    ps.print('(');
    int size = length();
    for (int i = 0; i < size; i++)
      {
	if (i != 0)
	  ps.print(" ");
	print(i, ps);
      }
    ps.print(")");
  }
}
