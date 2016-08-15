// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed or unsigned 8-bit integers (bytes). */

public abstract class ByteVector<E> extends PrimIntegerVector<E>
{
    byte[] data;
    protected static byte[] empty = new byte[0];

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void copyBuffer(int length) {
        int oldLength = data.length;
        if (length == -1)
            length = oldLength;
        if (oldLength != length) {
            byte[] tmp = new byte[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public byte[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (byte[]) buffer; }

    public final byte getByte(int index) {
        return data[effectiveIndex(index)];
    }

    public final byte getByteRaw(int index) {
        return data[index];
    }

    public final void setByte(int index, byte value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        data[effectiveIndex(index)] = value;
    }

    public final void setByteRaw(int index, byte value) {
        data[index] = value;
    }

    public void add(byte v) {
        int sz = size();
        addSpace(sz, 1);
        setByte(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        byte[] d = data;
        while (--count >= 0)
            d[start++] = 0;
    }

    public int readFrom(int start, int count, InputStream in)
        throws IOException {
        int pos = start;
        while (count > 0) {
            long result = getSegment(pos);
            int where = (int) result;
            int size = (int) (result >> 32);
            if (size > count)
                size = count;
            int n = in.read(data, where, size);
            if (n < 0) {
                if (pos == start)
                    return -1;
                break;
            }
            pos += n;
            count -= n;
        }
        return pos - start;
    }

    public void writeTo(OutputStream out)
            throws IOException {
        writeTo(0, size(), out);
    }
    public void writeTo(int start, int count, OutputStream out)
            throws IOException {
        while (count > 0) {
            long result = getSegment(start);
            int where = (int) result;
            int size = (int) (result >> 32);
            if (size > count)
                size = count;
            out.write(data, where, size);
            start += size;
            count -= size;
        }
    }

    public void copyFrom (int index, ByteVector src, int start, int end) {
        int count = end-start;
        int sz = size();
        int src_sz = src.size();
        if (count < 0 || index+count > sz || end > src_sz)
            throw new ArrayIndexOutOfBoundsException();
        int sseg, dseg;
        if ((sseg = src.getSegmentReadOnly(start, count)) >= 0 &&
            (dseg = getSegment(index, count)) >= 0) {
            System.arraycopy(src.data, sseg, data, dseg, count);
        } else {
            for (int i = 0; i < count; i++)
                setByte(index+i, src.getByte(start+i));
        }
    }

    public InputStream getInputStream() {
        int sz = size();
        int seg = getSegmentReadOnly(0, sz);
        if (seg >= 0)
            return new ByteArrayInputStream(data, seg, sz);
        else
            return new ByteVectorInputStream(this);
    }

    static class ByteVectorInputStream extends InputStream {
        ByteVector bvec;
        int pos;
        int mark;
        int size;
        public ByteVectorInputStream(ByteVector bvec) {
            this.bvec = bvec;
            this.size = bvec.size();
        }
        public int read() {
            return pos >= size ? -1 :
                (0xff & bvec.getByte(pos++));
        }
        public boolean markSupported() { return true; }
        public void mark(int readLimit) { mark = pos; }
        public void reset() { pos = mark; }
        public void close() { }
        public int available() { return size-pos; }
        public long skip(long n) {
            if (n < 0) n = 0;
            if (n < size-pos) { pos = size; return size-pos; }
            else { pos += n; return n; }
        }
    }

    /** Covert bytes, interpreted as UTF-8 characters, to a String. */
    public String toUtf8(int start, int length) {
        if (start+length>size()) throw new IndexOutOfBoundsException();
        int seg = getSegmentReadOnly(start, length);
        byte[] buf;
        if (seg >= 0) {
            buf = data;
            start = seg;
        } else {
            buf = new byte[length];
            for (int i = 0; i < length; i++)
                buf[i] = getByte(start+i);
        }
        return Strings.toUtf8(buf, start, length);
    }
}
