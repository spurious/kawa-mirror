package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of signed 16-bit integers. */

public class S16Vector extends UniformVector
{
  short[] data;

  public S16Vector(int num, short val)
  {
    short[] array = new short[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public S16Vector(int num)
  {
    data = new short[num];
  }

  public S16Vector(Sequence seq)
  {
    data = new short[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "s16"; }

  public final int length() { return data.length; }

  public final int intValue(int i)
  {
    return data[i];
  }

  public final short shortValue(int i)
  {
    return data[i];
  }

  public final Object get (int index)
  {
    return IntNum.make(data[index]);
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

}
