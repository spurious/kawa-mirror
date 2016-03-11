// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed or unsigned 32-bit integers (ints). */

public abstract class IntVector<E> extends PrimIntegerVector<E>
{
    int[] data;

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void copyBuffer(int length) {
        int oldLength = data.length;
        if (length == -1)
            length = oldLength;
        if (oldLength != length) {
            int[] tmp = new int[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public int[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (int[]) buffer; }

    public final int getInt(int index) {
        return data[effectiveIndex(index)];
    }

    public final int getIntRaw(int index) {
        return data[index];
    }

    public final void setInt(int index, int value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        data[effectiveIndex(index)] = value;
    }

    public final void setIntRaw(int index, int value) {
        data[index] = value;
    }

    public void add(int v) {
        int sz = size();
        addSpace(sz, 1);
        setInt(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        int[] d = data;
        while (--count >= 0)
            d[start++] = 0;
    }

}
