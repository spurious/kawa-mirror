package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of unsigned 32-bit integers. */

public class U32Vector extends UniformVector
{
  int[] data;

  public U32Vector(int num, int val)
  {
    int[] array = new int[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public U32Vector(int num)
  {
    data = new int[num];
  }

  public U32Vector(Sequence seq)
  {
    data = new int[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "u32"; }

  public final int length() { return data.length; }

  /** Returns element as an unsigned int. */
  public final int intValue(int i)
  {
    return data[i];
  }

  public final long longValue(int i)
  {
    return ((long) data[i]) & 0xffffffffL;
  }

  public final Object get (int index)
  {
    return IntNum.make(longValue(index));
  }

  public final void set(int index, int value)
  {
    data[index] = value;
  }

  public final void set(int index, long value)
  {
    data[index] = (int) value;
  }

  public final void setElementAt(Object value, int index)
  {
    data[index] = ((Number) value).intValue();
  }

  public final void print(int index, java.io.PrintWriter ps)
  {
    ps.print(longValue(index));
  }
}
