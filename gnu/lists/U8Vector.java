// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of unsigned 8-bit integers (bytes). */

public class U8Vector extends SimpleVector
  implements Externalizable
  /* BEGIN JAVA2 */
  , Comparable
  /* END JAVA2 */
{
  byte[] data;

  public U8Vector ()
  {
    data = S8Vector.empty;
  }

  public U8Vector(int size, byte value)
  {
    byte[] array = new byte[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public U8Vector(int size)
  {
    this.data = new byte[size];
    this.size = size;
  }

  public U8Vector (byte[] data)
  {
    this.data = data;
    size = data.length;
  }

  public U8Vector(Sequence seq)
  {
    data = new byte[seq.size()];
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
	byte[] tmp = new byte[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final byte byteAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return data[index];
  }

  public final byte byteAtBuffer(int index)
  {
    return data[index];
  }

  public final int intAtBuffer(int index)
  {
    return data[index] & 0xff;
  }

  public final Object get(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return Convert.toObjectUnsigned(data[index]);
  }

  public final Object getBuffer(int index)
  {
    return Convert.toObjectUnsigned(data[index]);
  }

  public Object setBuffer(int index, Object value)
  {
    byte old = data[index];
    data[index] = Convert.toByteUnsigned(value);
    return Convert.toObjectUnsigned(old);
  }

  public final void setByteAt(int index, byte value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setByteAtBuffer(int index, byte value)
  {
    data[index] = value;
  }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = 0;
  }

  public int getElementKind()
  {
    return INT_U8_VALUE;
  }

  public String getTag() { return "u8"; }

  public boolean consumeNext (int ipos, Consumer out)
  {
    int index = ipos >>> 1;
    if (index >= size)
      return false;
    out.writeInt(data[index] & 0xff);
    return true;
  }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeInt(data[i] & 0xff);
  }

  public int compareTo(Object obj)
  {
    return compareToInt(this, (U8Vector) obj);
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeByte).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeByte(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    byte[] data = new byte[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readByte();
    this.data = data;
    this.size = size;
  }
}
