package gnu.kawa.util;
import gnu.math.*;
import java.io.*;

/** Uniform vector of signed 8-bit integers. */

public class S8Vector extends UniformVector implements Externalizable
{
  byte[] data;

  public S8Vector ()
  {
  }

  public S8Vector(int num, byte val)
  {
    byte[] array = new byte[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public S8Vector(int num)
  {
    data = new byte[num];
  }

  public S8Vector(Sequence seq)
  {
    data = new byte[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "s8"; }

  public final int length() { return data.length; }

  public final int intValue(int i)
  {
    return data[i];
  }

  public final byte byteValue(int i)
  {
    return data[i];
  }

  public final Object get (int index)
  {
    return IntNum.make(data[index]);
  }

  public final void set(int index, byte value)
  {
    data[index] = value;
  }

  public final void setElementAt (Object value, int index)
  {
    data[index] = (byte) ((Number) value).intValue();
  }

  public final void print(int index, java.io.PrintWriter ps)
  {
    ps.print(intValue(index));
  }

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeByte).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = data.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeByte(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    byte[] data = new byte[len];
    for (int i = 0;  i < len;  i++)
      data[i] = in.readByte();
    this.data = data;
  }
}
