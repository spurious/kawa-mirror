package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of 32-bit floats. */

public class F32Vector extends UniformVector
{
  float[] data;

  public F32Vector(int num, float val)
  {
    float[] array = new float[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public F32Vector(int num)
  {
    data = new float[num];
  }

  public F32Vector(Sequence seq)
  {
    data = new float[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "f32"; }

  public final int length() { return data.length; }

  public final float floatValue(int i)
  {
    return data[i];
  }

  public final Object get (int index)
  {
    return DFloNum.make(data[index]);
  }

  public final void set(int index, float value)
  {
    data[index] = value;
  }

  public final void setElementAt(Object value, int index)
  {
    data[index] = ((Number) value).floatValue();
  }

  public final void print(int index, java.io.PrintWriter ps)
  {
    ps.print(floatValue(index));
  }
}
