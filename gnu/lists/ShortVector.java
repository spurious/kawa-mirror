// Copyright (c) 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import java.io.*;

/** Simple adjustable-length vector of 32-bit integers (signed or unsigned). */

public abstract class ShortVector<E> extends PrimIntegerVector<E>
{
    short[] data;

    protected static short[] empty = new short[0];

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void setBufferLength(int length) {
        int oldLength = data.length;
        if (oldLength != length) {
            short[] tmp = new short[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    protected Object getBuffer() { return data; }

    public final short shortAt(int index) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        return data[index];
    }

    public final short shortAtBuffer(int index) {
        return data[index];
    }

    public final void setShortAt(int index, short value) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        data[index] = value;
    }

    public final void setShortAtBuffer(int index, short value) {
        data[index] = value;
    }

    public void add(short v) {
        int sz = size;
        addSpace(sz, 1);
        setShortAt(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        while (--count >= 0)
            data[start++] = 0;
    }

    /**
     * @serialData Write 'size' (using writeInt),
     *   followed by 'size' elements in order (using writeShort).
     */
    public void writeExternal(ObjectOutput out) throws IOException {
        int size = this.size;
        out.writeInt(size);
        for (int i = 0;  i < size;  i++)
            out.writeShort(data[i]);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        int size = in.readInt();
        short[] data = new short[size];
        for (int i = 0;  i < size;  i++)
            data[i] = in.readShort();
        this.data = data;
        this.size = size;
    }
}
