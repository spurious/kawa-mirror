package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of signed 32-bit integers. */

public class S32Vector extends UniformVector
{
  int[] data;

  public S32Vector(int num, int val)
  {
    int[] array = new int[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public S32Vector(int num)
  {
    data = new int[num];
  }

  public S32Vector(Sequence seq)
  {
    data = new int[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "s32"; }

  public final int length() { return data.length; }

  public final int intValue(int i)
  {
    return data[i];
  }

  public final Object get (int index)
  {
    return IntNum.make(data[index]);
  }

  public final void set(int index, int value)
  {
    data[index] = value;
  }

  public final void setElementAt(Object value, int index)
  {
    data[index] = ((Number) value).intValue();
  }

  public final void print(int index, java.io.PrintWriter ps)
  {
    ps.print(intValue(index));
  }
}
