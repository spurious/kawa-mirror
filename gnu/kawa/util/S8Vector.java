package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of signed 8-bit integers. */

public class S8Vector extends UniformVector
{
  byte[] data;

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

}
