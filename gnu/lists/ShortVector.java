// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed or unsigned 16-bit integers (shorts). */

public abstract class ShortVector<E> extends PrimIntegerVector<E>
{
    short[] data;
    protected static short[] empty = new short[0];

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void copyBuffer(int length) {
        int oldLength = data.length;
        if (length == -1)
            length = oldLength;
        if (oldLength != length) {
            short[] tmp = new short[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public short[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (short[]) buffer; }

    public final short getShort(int index) {
        return data[effectiveIndex(index)];
    }

    public final short getShortRaw(int index) {
        return data[index];
    }

    public final void setShort(int index, short value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        data[effectiveIndex(index)] = value;
    }

    public final void setShortRaw(int index, short value) {
        data[index] = value;
    }

    public void add(short v) {
        int sz = size();
        addSpace(sz, 1);
        setShort(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        short[] d = data;
        while (--count >= 0)
            d[start++] = 0;
    }

}
