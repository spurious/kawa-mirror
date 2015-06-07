// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.math.ULong;
import java.io.*;

/** Simple adjustable-length vector of unsigned 64-bit integers (longs). */

public class U64Vector extends LongVector<ULong>
{
    public U64Vector() {
        data = empty;
    }

    public U64Vector(int size, long value) {
        long[] array = new long[size];
        data = array;
        this.size = size;
        while (--size >= 0)
            array[size] = value;
    }

    public U64Vector(int size) {
        this(new long[size]);
    }

    public U64Vector(long[] data) {
        this.data = data;
        size = data.length;
    }

    public U64Vector(Sequence seq) {
        data = new long[seq.size()];
        addAll(seq);
    }

    public final ULong get(int index) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        return ULong.valueOf(data[index]);
    }

    public final ULong getBuffer(int index) {
        return ULong.valueOf(data[index]);
    }

  @Override
  public void setBuffer(int index, ULong value)
  {
    data[index] = value.longValue();
  }

  public int getElementKind()
  {
    return INT_U64_VALUE;
  }

  public String getTag() { return "u64"; }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = iposStart >>> 1;
        int end = iposEnd >>> 1;
        if (end > size)
            end = size;
        for (;  i < end;  i++) {
            long v = data[i];
            if (v >= 0)
                out.writeLong(v);
            else
                out.writeObject(ULong.valueOf(v));
        }
    }

  public int compareTo(Object obj)
  {
    U64Vector vec2 = (U64Vector) obj;
    long[] arr1 = data;
    long[] arr2 = vec2.data;
    int n1 = size;
    int n2 = vec2.size;
    int n = n1 > n2 ? n2 : n1;
    for (int i = 0;  i < n;  i++)
      {
	long v1 = arr1[i];
	long v2 = arr2[i];
	if (v1 != v2)
	  return (v1^0x8000000000000000L) > (v2^0x8000000000000000L) ? 1 : -1;
      }
    return n1 - n2;
  }
}
