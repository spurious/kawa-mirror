package gnu.kawa.util;
import gnu.math.*;
import java.io.*;

/** Uniform vector of 64-bit doubles. */

public class F64Vector extends UniformVector implements Externalizable
{
  double[] data;

  public F64Vector ()
  {
  }

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

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeDouble).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = data.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeDouble(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    double[] data = new double[len];
    for (int i = 0;  i < len;  i++)
      data[i] = in.readDouble();
    this.data = data;
  }
}
