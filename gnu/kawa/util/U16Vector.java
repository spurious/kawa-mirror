package gnu.kawa.util;
import gnu.math.*;
import java.io.*;

/** Uniform vector of unsigned 16-bit integers. */

public class U16Vector extends UniformVector implements Externalizable
{
  short[] data;

  public U16Vector ()
  {
  }

  public U16Vector(int num, short val)
  {
    short[] array = new short[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public U16Vector(int num)
  {
    data = new short[num];
  }

  public U16Vector(Sequence seq)
  {
    data = new short[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "s16"; }

  public final int length() { return data.length; }

  public final int intValue(int i)
  {
    return data[i] & 0xffff;
  }

  public final Object get (int index)
  {
    return IntNum.make(data[index] & 0xffff);
  }

  public final void set(int index, short value)
  {
    data[index] = value;
  }

  public final void setElementAt (Object value, int index)
  {
    data[index] = (short) ((Number) value).intValue();
  }

  public final void print(int index, java.io.PrintWriter ps)
  {
    ps.print(intValue(index));
  }

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeShort).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = data.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeShort(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    short[] data = new short[len];
    for (int i = 0;  i < len;  i++)
      data[i] = in.readShort();
    this.data = data;
  }
}
