// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed or unsigned 64-bit integers (longs). */

public abstract class LongVector<E> extends PrimIntegerVector<E>
{
    long[] data;
    protected static long[] empty = new long[0];

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void setBufferLength(int length) {
        int oldLength = data.length;
        if (oldLength != length) {
            long[] tmp = new long[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public long[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (long[]) buffer; }

    public final long longAt(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return data[index];
    }

    public final long longAtBuffer(int index) {
        return data[index];
    }

    public final int intAtBuffer(int index) {
        return (int) data[index];
    }

    public final void setLongAt(int index, long value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        if (indexes != null)
            index = indexes.intAt(index);
        data[index] = value;
    }

    public final void setLongAtBuffer(int index, long value) {
        data[index] = value;
    }

    public void add(long v) {
        int sz = size();
        addSpace(sz, 1);
        setLongAt(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        long[] d = data;
        while (--count >= 0)
            d[start++] = 0;
    }

}
