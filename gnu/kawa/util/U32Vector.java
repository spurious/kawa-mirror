package gnu.kawa.util;
import gnu.math.*;
import java.io.*;

/** Uniform vector of unsigned 32-bit integers. */

public class U32Vector extends UniformVector implements Externalizable
{
  int[] data;

  public U32Vector ()
  {
  }

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

  public U32Vector (int[] data)
  {
    this.data = data;
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

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeInt).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = data.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeInt(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    int[] data = new int[len];
    for (int i = 0;  i < len;  i++)
      data[i] = in.readInt();
    this.data = data;
  }
}
