// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of Boolean values. */

public  class BitVector extends SimpleVector<Boolean>
    implements Comparable
{
    boolean[] data;
    protected static boolean[] empty = new boolean[0];

    public BitVector() {
        data = empty;
    }

    public BitVector(int size, boolean value) {
        boolean[] array = new boolean[size];
        data = array;
        if (value != false) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public BitVector(int size) {
        this(new boolean[size]);
    }

    /** Reuses the argument without making a copy. */
    public BitVector(boolean[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public BitVector(boolean[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void copyBuffer(int length) {
        int oldLength = data.length;
        if (length == -1)
            length = oldLength;
        if (oldLength != length) {
            boolean[] tmp = new boolean[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public boolean[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (boolean[]) buffer; }

    public final boolean getBoolean(int index) {
        return data[effectiveIndex(index)];
    }

    public final boolean getBooleanRaw(int index) {
        return data[index];
    }

    public final Boolean get(int index) {
        return Boolean.valueOf(data[effectiveIndex(index)]);
    }

    public final Boolean getRaw(int index) {
        return Boolean.valueOf(data[index]);
    }

    public final void setBoolean(int index, boolean value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        data[effectiveIndex(index)] = value;
    }

    public final void setBooleanRaw(int index, boolean value) {
        data[index] = value;
    }

    @Override
    public final void setRaw(int index, Boolean value) {
        data[index] = value.booleanValue();
    }

    public void add(boolean v) {
        int sz = size();
        addSpace(sz, 1);
        setBoolean(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        boolean[] d = data;
        while (--count >= 0)
            d[start++] = false;
    }

    @Override
    protected BitVector newInstance(int newLength) {
        return new BitVector(newLength < 0 ? data : new boolean[newLength]);
    }

    public int getElementKind() { return BOOLEAN_VALUE; }

    public String getTag() { return "b"; }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = nextIndex(iposStart);
        int end = nextIndex(iposEnd);
        for (;  i < end;  i++)
            out.writeBoolean(getBoolean(i));
    }

    public int compareTo(Object obj) {
        BitVector vec2 = (BitVector) obj;
        boolean[] arr1 = data;
        boolean[] arr2 = vec2.data;
        int n1 = size();
        int n2 = vec2.size();
        int n = n1 > n2 ? n2 : n1;
        for (int i = 0;  i < n;  i++) {
            boolean v1 = arr1[effectiveIndex(i)];
            boolean v2 = arr2[effectiveIndex(i)];
            if (v1 != v2)
                return v1 && ! v2 ? 1 : -1;
        }
        return n1 - n2;
    }

}
