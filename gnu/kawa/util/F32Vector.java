package gnu.kawa.util;
import gnu.math.*;
import java.io.*;

/** Uniform vector of 32-bit floats. */

public class F32Vector extends UniformVector implements Externalizable
{
  float[] data;

  public F32Vector ()
  {
  }

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

  public F32Vector (float[] data)
  {
    this.data = data;
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

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeFloat).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = data.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeFloat(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    float[] data = new float[len];
    for (int i = 0;  i < len;  i++)
      data[i] = in.readFloat();
    this.data = data;
  }
}
