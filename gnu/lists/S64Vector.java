// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 64-bit integers (longs). */

public  class S64Vector extends LongVector<Long>
{
    public S64Vector() {
        data = empty;
    }

    public S64Vector(int size, long value) {
        long[] array = new long[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public S64Vector(int size) {
        this(new long[size]);
    }

    /** Reuses the argument without making a copy. */
    public S64Vector(long[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public S64Vector(long[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final Long get(int index) {
        return Long.valueOf(data[effectiveIndex(index)]);
    }

    public final Long getRaw(int index) {
        return Long.valueOf(data[index]);
    }

    @Override
    public final void setRaw(int index, Long value) {
        data[index] = value.longValue();
    }

    @Override
    protected S64Vector newInstance(int newLength) {
        return new S64Vector(newLength < 0 ? data : new long[newLength]);
    }

    public int getElementKind() { return INT_S64_VALUE; }

    public String getTag() { return "s64"; }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = nextIndex(iposStart);
        int end = nextIndex(iposEnd);
        for (;  i < end;  i++)
            out.writeLong(getLong(i));
    }

    public int compareTo(Object obj) {
        S64Vector vec2 = (S64Vector) obj;
        long[] arr1 = data;
        long[] arr2 = vec2.data;
        int n1 = size();
        int n2 = vec2.size();
        int n = n1 > n2 ? n2 : n1;
        for (int i = 0;  i < n;  i++) {
            long v1 = arr1[effectiveIndex(i)];
            long v2 = arr2[effectiveIndex(i)];
            if (v1 != v2)
                return v1 > v2 ? 1 : -1;
        }
        return n1 - n2;
    }

}
