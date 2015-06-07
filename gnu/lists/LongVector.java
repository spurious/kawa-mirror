// Copyright (c) 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of 64-bit integers (signed or unsigned). */

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

    protected Object getBuffer() { return data; }

    public final long longAt(int index) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        return data[index];
    }

    public final long longAtBuffer(int index) {
        return data[index];
    }

    public final int intAtBuffer(int index) {
        return (int) data[index];
    }

    public final void setLongAt(int index, long value) {
        if (index >= size)
            throw new IndexOutOfBoundsException();
        data[index] = value;
    }

    public final void setLongAtBuffer(int index, long value) {
        data[index] = value;
    }

    public void add(long v) {
        int sz = size;
        addSpace(sz, 1);
        setLongAt(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        while (--count >= 0)
            data[start++] = 0;
    }

    /**
     * @serialData Write 'size' (using writeInt),
     *   followed by 'size' elements in order (using writeLong).
     */
    public void writeExternal(ObjectOutput out) throws IOException {
        int size = this.size;
        out.writeInt(size);
        for (int i = 0;  i < size;  i++)
            out.writeLong(data[i]);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        int size = in.readInt();
        long[] data = new long[size];
        for (int i = 0;  i < size;  i++)
            data[i] = in.readLong();
        this.data = data;
        this.size = size;
    }
}
