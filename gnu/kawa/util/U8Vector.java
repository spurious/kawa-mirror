package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of unsigned 8-bit integers. */

public class U8Vector extends UniformVector
{
  byte[] data;

  public U8Vector(int num, byte val)
  {
    byte[] array = new byte[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public U8Vector(int num)
  {
    data = new byte[num];
  }

  public U8Vector(Sequence seq)
  {
    data = new byte[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "s8"; }

  public final int length() { return data.length; }

  public final int intValue(int i)
  {
    return data[i] & 0xff;
  }

  public final Object get (int index)
  {
    return IntNum.make(data[index] & 0xff);
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

}
