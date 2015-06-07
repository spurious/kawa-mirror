// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 16-bit integers (shorts). */

public class S16Vector extends ShortVector<Short>
{
    public S16Vector() {
        data = empty;
    }

  public S16Vector(int size, short value)
  {
    short[] array = new short[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public S16Vector(int size)
  {
    this.data = new short[size];
    this.size = size;
  }

  public S16Vector (short[] data)
  {
    this.data = data;
    size = data.length;
  }

  public S16Vector(Sequence seq)
  {
    data = new short[seq.size()];
    addAll(seq);
  }

  public final int intAtBuffer(int index)
  {
    return data[index];
  }

  public final Short get(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return Short.valueOf(data[index]);
  }

  public final Short getBuffer(int index)
  {
    return Short.valueOf(data[index]);
  }

  @Override
  public void setBuffer(int index, Short value)
  {
    data[index] = value.shortValue();
  }

  public int getElementKind()
  {
    return INT_S16_VALUE;
  }

  public String getTag() { return "s16"; }

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
    return compareToInt(this, (S16Vector) obj);
  }

}
