// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.math.UInt;
import java.io.*;

/** Simple adjustable-length vector of unsigned 32-bit integers (ints). */

public class U32Vector extends IntVector<UInt>
{
  public U32Vector ()
  {
    data = empty;
  }

  public U32Vector(int size, int value)
  {
    int[] array = new int[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

    public U32Vector(int size) {
        this(new int[size]);
    }

    public U32Vector(int[] data) {
        this.data = data;
        size = data.length;
    }

  public U32Vector(Sequence seq)
  {
    data = new int[seq.size()];
    addAll(seq);
  }

  public final long longAtBuffer(int index)
  {
    return (long) data[index] & 0xffffffffL;
  }

  public final UInt get(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return UInt.valueOf(data[index]);
  }

  public final UInt getBuffer(int index)
  {
    return UInt.valueOf(data[index]);
  }

  @Override
  public void setBuffer(int index, UInt value)
  {
    data[index] = value.intValue();
  }

  public int getElementKind()
  {
    return INT_U16_VALUE;
  }

  public String getTag() { return "u32"; }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeInt(data[i]);
  }

  public int compareTo(Object obj)
  {
    return compareToLong(this, (U32Vector) obj);
  }
}
