// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of 32-bit floats. */

public  class F32Vector extends SimpleVector<Float>
    implements Comparable
{
    float[] data;
    protected static float[] empty = new float[0];

    public F32Vector() {
        data = empty;
    }

    public F32Vector(int size, float value) {
        float[] array = new float[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public F32Vector(int size) {
        this(new float[size]);
    }

    /** Reuses the argument without making a copy. */
    public F32Vector(float[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public F32Vector(float[] values, int offset, int length) {
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
            float[] tmp = new float[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public float[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (float[]) buffer; }

    public final float getFloat(int index) {
        return data[effectiveIndex(index)];
    }

    public final float getFloatRaw(int index) {
        return data[index];
    }

    public final Float get(int index) {
        return Float.valueOf(data[effectiveIndex(index)]);
    }

    public final Float getRaw(int index) {
        return Float.valueOf(data[index]);
    }

    public final void setFloat(int index, float value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        data[effectiveIndex(index)] = value;
    }

    public final void setFloatRaw(int index, float value) {
        data[index] = value;
    }

    @Override
    public final void setRaw(int index, Float value) {
        data[index] = value.floatValue();
    }

    public void add(float v) {
        int sz = size();
        addSpace(sz, 1);
        setFloat(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        float[] d = data;
        while (--count >= 0)
            d[start++] = 0;
    }

    @Override
    protected F32Vector newInstance(int newLength) {
        return new F32Vector(newLength < 0 ? data : new float[newLength]);
    }

    public int getElementKind() { return FLOAT_VALUE; }

    public String getTag() { return "f32"; }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = nextIndex(iposStart);
        int end = nextIndex(iposEnd);
        for (;  i < end;  i++)
            out.writeFloat(getFloat(i));
    }

    public int compareTo(Object obj) {
        F32Vector vec2 = (F32Vector) obj;
        float[] arr1 = data;
        float[] arr2 = vec2.data;
        int n1 = size();
        int n2 = vec2.size();
        int n = n1 > n2 ? n2 : n1;
        for (int i = 0;  i < n;  i++) {
            float v1 = arr1[effectiveIndex(i)];
            float v2 = arr2[effectiveIndex(i)];
            if (v1 != v2)
                return v1 > v2 ? 1 : -1;
        }
        return n1 - n2;
    }

}
