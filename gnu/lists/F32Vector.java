// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector whose elements are 32-bit floats. */

public class F32Vector extends SimpleVector  implements Externalizable
{
  float[] data;
  protected static float[] empty = new float[0];

  public F32Vector ()
  {
    data = empty;
  }

  public F32Vector(int size, float value)
  {
    float[] array = new float[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public F32Vector(int size)
  {
    this.data = new float[size];
    this.size = size;
  }

  public F32Vector (float[] data)
  {
    this.data = data;
    size = data.length;
  }

  public F32Vector(Sequence seq)
  {
    data = new float[seq.size()];
    addAll(seq);
  }

  /** Get the allocated length of the data buffer. */
  public int getBufferLength()
  {
    return data.length;
  }

  public void setBufferLength(int length)
  {
    int oldLength = data.length;
    if (oldLength != length)
      {
	float[] tmp = new float[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
 }

  protected Object getBuffer() { return data; }

  public final int intAtBuffer(int index)
  {
    return (int) data[index];
  }

  public final float floatAt(int index)
  {
    if (index >= size)
      throw new ArrayIndexOutOfBoundsException();
    return data[index];
  }

  public final float floatAtBuffer(int index)
  {
    return data[index];
  }

  public final Object get (int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return Convert.toObject(data[index]);
  }

  public final Object getBuffer (int index)
  {
    return Convert.toObject(data[index]);
  }

  public final void setFloatAt(int index, float value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setFloatAtBuffer(int index, float value)
  {
    data[index] = value;
  }

  public final Object setBuffer(int index, Object value)
  {
    Object old = Convert.toObject(data[index]);
    data[index] = Convert.toFloat(value);
    return old;
  }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = 0;
  }

  public int getElementKind()
  {
    return FLOAT_VALUE;
  }

  public String getTag() { return "f32"; }

  public boolean consumeNext(int ipos, Object xpos, Consumer out)
  {
    int index = ipos >>> 1;
    if (index >= size)
      return false;
    out.writeFloat(data[index]);
    return true;
  }

  public void consume(int iposStart, Object xposStart,
		      int iposEnd, Object xposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    for (;  i < end;  i++)
      out.writeFloat(data[i]);
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeFloat).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeFloat(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    float[] data = new float[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readFloat();
    this.data = data;
    this.size = size;
  }
}
