package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of unsigned 64-bit integers. */

public class U64Vector extends UniformVector
{
  long[] data;

  public U64Vector(int num, long val)
  {
    long[] array = new long[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public U64Vector(int num)
  {
    data = new long[num];
  }

  public U64Vector(Sequence seq)
  {
    data = new long[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "u64"; }

  public final int length() { return data.length; }

  public final long longValue(int i)
  {
    return data[i];
  }

  public final Object get (int index)
  {
    return IntNum.makeU(data[index]);
  }

  public final void set(int index, long value)
  {
    data[index] = value;
  }

  public final void setElementAt(Object value, int index)
  {
    data[index] = ((Number) value).longValue();
  }

  public final void print(int index, java.io.PrintWriter ps)
  {
    long value = data[index];
    if (value >= 0)
      ps.print(value);
    else
      ps.print(IntNum.makeU(value));
  }
}
