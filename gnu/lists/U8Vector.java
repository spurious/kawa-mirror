// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of unsigned 8-bit integers (bytes). */

public class U8Vector extends ByteVector
{
  public U8Vector ()
  {
    data = ByteVector.empty;
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

  public int getElementKind()
  {
    return INT_U8_VALUE;
  }

  public String getTag() { return "u8"; }

  public int compareTo(Object obj)
  {
    return compareToInt(this, (U8Vector) obj);
  }
}
