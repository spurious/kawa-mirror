// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;
import gnu.math.UShort;

/** Simple adjustable-length vector of unsigned 16-bit integers (shorts). */

public class U16Vector extends ShortVector<UShort>
{
    public U16Vector() {
        data = empty;
    }

  public U16Vector(int size, short value)
  {
    short[] array = new short[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public U16Vector(int size)
  {
    this.data = new short[size];
    this.size = size;
  }

  public U16Vector (short[] data)
  {
    this.data = data;
    size = data.length;
  }

  public U16Vector(Sequence seq)
  {
    data = new short[seq.size()];
    addAll(seq);
  }

  public final int intAtBuffer(int index)
  {
    return data[index] & 0xffff;
  }

  public final UShort get(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return UShort.valueOf(data[index]);
  }

  public final UShort getBuffer(int index)
  {
    return UShort.valueOf(data[index]);
  }

  @Override
  public void setBuffer(int index, UShort value)
  {
    data[index] = value.shortValue();
  }

  public int getElementKind()
  {
    return INT_U16_VALUE;
  }

  public String getTag() { return "u16"; }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeInt(data[i] & 0xffff);
  }

  public int compareTo(Object obj)
  {
    return compareToInt(this, (U16Vector) obj);
  }

}
