package gnu.kawa.util;
import gnu.math.*;

/** Uniform vector of 64-bit doubles. */

public class F64Vector extends UniformVector
{
  double[] data;

  public F64Vector(int num, double val)
  {
    double[] array = new double[num];
    data = array;
    while (--num >= 0)
      array[num] = val;
  }

  public F64Vector(int num)
  {
    data = new double[num];
  }

  public F64Vector(Sequence seq)
  {
    data = new double[seq.length()];
    copy(seq);
  }

  public final String getTag() { return "f64"; }

  public final int length() { return data.length; }

  public final double doubleValue(int i)
  {
    return data[i];
  }

  public final Object get (int index)
  {
    return DFloNum.make(data[index]);
  }

  public final void set(int index, double value)
  {
    data[index] = value;
  }

  public final void setElementAt(Object value, int index)
  {
    data[index] = ((Number) value).doubleValue();
  }

  public final void print(int index, java.io.PrintWriter ps)
  {
    ps.print(doubleValue(index));
  }
}
