// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of 64-bit doubles. */

public  class F64Vector extends SimpleVector<Double>
    implements Comparable
{
    double[] data;
    protected static double[] empty = new double[0];

    public F64Vector() {
        data = empty;
    }

    public F64Vector(int size, double value) {
        double[] array = new double[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public F64Vector(int size) {
        this(new double[size]);
    }

    /** Reuses the argument without making a copy. */
    public F64Vector(double[] data) {
        this.data = data;
    }

    /*
    public F64Vector(Sequence seq) {
        data = new double[seq.size()];
        addAll(seq);
    }
    */

    public F64Vector(double[] data, IntSequence indexes) {
        this.data = data;
        this.indexes = indexes;
    }

    /** Makes a copy of (part of) the argument array. */
    public F64Vector(double[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void setBufferLength(int length) {
        int oldLength = data.length;
        if (oldLength != length) {
            double[] tmp = new double[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public double[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (double[]) buffer; }

    public final double doubleAt(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return data[index];
    }

    public final double doubleAtBuffer(int index) {
        return data[index];
    }

    public final Double get(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return Double.valueOf(data[index]);
    }

    public final Double getBuffer(int index) {
        return Double.valueOf(data[index]);
    }

    public final void setDoubleAt(int index, double value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        if (indexes != null)
            index = indexes.intAt(index);
        data[index] = value;
    }

    public final void setDoubleAtBuffer(int index, double value) {
        data[index] = value;
    }

    @Override
    public final void setBuffer(int index, Double value) {
        data[index] = value.doubleValue();
    }

    public void add(double v) {
        int sz = size();
        addSpace(sz, 1);
        setDoubleAt(sz, v);
    }

    protected void clearBuffer(int start, int count) {
        double[] d = data;
        while (--count >= 0)
            d[start++] = 0;
    }

    public int getElementKind() { return DOUBLE_VALUE; }

    public String getTag() { return "f64"; }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = nextIndex(iposStart);
        int end = nextIndex(iposEnd);
        for (;  i < end;  i++)
            out.writeDouble(doubleAt(i));
    }

    public int compareTo(Object obj) {
        F64Vector vec2 = (F64Vector) obj;
        double[] arr1 = data;
        double[] arr2 = vec2.data;
        int n1 = size();
        int n2 = vec2.size();
        IntSequence inds1 = getIndexesForce();
        IntSequence inds2 = vec2.getIndexesForce();
        int n = n1 > n2 ? n2 : n1;
        for (int i = 0;  i < n;  i++) {
            double v1 = arr1[inds1.intAt(i)];
            double v2 = arr2[inds2.intAt(i)];
            if (v1 != v2)
                return v1 > v2 ? 1 : -1;
        }
        return n1 - n2;
    }

}
