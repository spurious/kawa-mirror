// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 32-bit integers (ints). */

public class S32Vector extends IntVector<Integer> implements IntSequence
{
  public S32Vector ()
  {
    data = empty;
  }

  public S32Vector(int size, int value)
  {
    int[] array = new int[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

    public S32Vector(int size) {
        this(new int[size]);
    }

    public S32Vector(int[] data) {
        this.data = data;
        size = data.length;
    }

  public S32Vector(Sequence seq)
  {
    data = new int[seq.size()];
    addAll(seq);
  }

  public final long longAtBuffer(int index)
  {
    return (long) data[index];
  }

  public final Integer get(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return Integer.valueOf(data[index]);
  }

  public final Integer getBuffer(int index)
  {
    return Integer.valueOf(data[index]);
  }

  @Override
  public void setBuffer(int index, Integer value)
  {
    data[index] = value.intValue();
  }

  public int getElementKind()
  {
    return INT_S32_VALUE;
  }

  public String getTag() { return "s32"; }

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
    return compareToInt(this, (S32Vector) obj);
  }
}
