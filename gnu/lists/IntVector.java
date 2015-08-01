// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed or unsigned 32-bit integers (ints). */

public abstract class IntVector<E> extends PrimIntegerVector<E>
{
    int[] data;
    protected static int[] empty = new int[0];

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void setBufferLength(int length) {
        int oldLength = data.length;
        if (oldLength != length) {
            int[] tmp = new int[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public int[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (int[]) buffer; }

    public final int intAt(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return data[index];
    }

    public final int intAtBuffer(int index) {
        return data[index];
    }

    public final void setIntAt(int index, int value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        if (indexes != null)
            index = indexes.intAt(index);
        data[index] = value;
    }

    public final void setIntAtBuffer(int index, int value) {
        data[index] = value;
    }

    public void add(int v) {
        int sz = size();
        addSpace(sz, 1);
        setIntAt(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        int[] d = data;
        while (--count >= 0)
            d[start++] = 0;
    }

}
