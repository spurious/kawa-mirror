// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 64-bit integers (longs). */

public class S64Vector extends LongVector<Long>
{
    public S64Vector() {
        data = empty;
    }

    public S64Vector(int size, long value) {
        long[] array = new long[size];
        data = array;
        this.size = size;
        while (--size >= 0)
            array[size] = value;
    }

    public S64Vector(int size) {
        this(new long[size]);
    }

    public S64Vector(long[] data) {
        this.data = data;
        size = data.length;
    }

    public S64Vector(Sequence seq) {
        data = new long[seq.size()];
        addAll(seq);
    }

    public final Long get(int index) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        return Long.valueOf(data[index]);
    }

    public final Long getBuffer(int index) {
        return Long.valueOf(data[index]);
    }

    @Override
    public void setBuffer(int index, Long value) {
        data[index] = value.longValue();
    }

  public int getElementKind()
  {
    return INT_S64_VALUE;
  }

  public String getTag() { return "s64"; }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeLong(data[i]);
  }

  public int compareTo(Object obj)
  {
    return compareToLong(this, (S64Vector) obj);
  }

}
