// Copyright (c) 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import java.io.*;

/** Simple adjustable-length vector of 32-bit integers (signed or unsigned). */

public abstract class IntVector<E> extends PrimIntegerVector<E>
{
    int[] data;

    protected static int[] empty = new int[0];

    /** Get the allocated length of the data buffer. */
    public int getBufferLength()
    {
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

    protected Object getBuffer() { return data; }

    public final int intAt(int index) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        return data[index];
    }

    public final int intAtBuffer(int index) {
        return data[index];
    }

    public final long longAt(int index) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        return longAtBuffer(index);
    }

    public final void setIntAt(int index, int value) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        data[index] = value;
    }

    public final void setIntAtBuffer(int index, int value) {
        data[index] = value;
    }

    public void add(int v) {
        int sz = size;
        addSpace(sz, 1);
        setIntAt(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        while (--count >= 0)
            data[start++] = 0;
    }

    /**
     * @serialData Write 'size' (using writeInt),
     *   followed by 'size' elements in order (using writeInt).
     */
    public void writeExternal(ObjectOutput out) throws IOException {
        int size = this.size;
        out.writeInt(size);
        for (int i = 0;  i < size;  i++)
            out.writeInt(data[i]);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        int size = in.readInt();
        int[] data = new int[size];
        for (int i = 0;  i < size;  i++)
            data[i] = in.readInt();
        this.data = data;
        this.size = size;
    }
}
