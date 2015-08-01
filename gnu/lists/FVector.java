// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of objects. */

public  class FVector<E> extends SimpleVector<E>
    implements Consumable, Comparable
{
    Object[] data;
    protected static Object[] empty = new Object[0];

    public FVector() {
        data = empty;
    }

    public FVector(int size, Object value) {
        Object[] array = new Object[size];
        data = array;
        if (value != null) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public FVector(int size) {
        this(new Object[size]);
    }

    /** Reuses the argument without making a copy. */
    public FVector(Object[] data) {
        this.data = data;
    }

    /*
    public FVector(Sequence seq) {
        data = new Object[seq.size()];
        addAll(seq);
    }
    */

    public FVector(Object[] data, IntSequence indexes) {
        this.data = data;
        this.indexes = indexes;
    }

    /** Makes a copy of (part of) the argument array. */
    public FVector(Object[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public FVector(java.util.List seq) {
        this.data = new Object[seq.size()];
        int i = 0;
        for (java.util.Iterator<? extends E> it = seq.iterator();  it.hasNext(); )
            data[i++] = it.next();
    }

    public static FVector make(Object... data) {
        return new FVector(data);
    }

    public void copyFrom (int index, FVector<E> src, int start, int end) {
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
                set(index+i, src.get(start+i));
        }
    }

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void setBufferLength(int length) {
        int oldLength = data.length;
        if (oldLength != length) {
            Object[] tmp = new Object[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public Object[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (Object[]) buffer; }

    public final E get(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return (E) data[index];
    }

    public final E getBuffer(int index) {
        return (E) data[index];
    }

    @Override
    public final void setBuffer(int index, Object value) {
        data[index] = value;
    }

    protected void clearBuffer(int start, int count) {
        Object[] d = data;
        while (--count >= 0)
            d[start++] = null;
    }

    public final void fill(int start, int end, E new_value) {
        if (indexes == null)
            java.util.Arrays.fill(data, start, end, new_value);
        else
            super.fill(start, end, new_value);
    }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = nextIndex(iposStart);
        int end = nextIndex(iposEnd);
        for (;  i < end;  i++)
            out.writeObject(get(i));
    }

    public void consume(Consumer out) {
        out.startElement("#vector");
        int len = size();
        for (int i = 0;  i < len;  i++)
            out.writeObject(get(i));
        out.endElement();
    }

    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof FVector))
            return false;
        FVector obj_vec = (FVector) obj;
        int n = size();
        if (obj_vec.data == null || obj_vec.size() != n)
            return false;
        Object[] this_data = data;
        Object[] obj_data = obj_vec.data;
        IntSequence inds1 = getIndexesForce();
        IntSequence inds2 = obj_vec.getIndexesForce();
        for (int i = 0;  i < n;  i++) {
            if (! (this_data[inds1.intAt(i)].equals(obj_data[inds2.intAt(i)])))
                return false;
        }
        return true;
    }

    public int compareTo(Object obj) {
        FVector vec2 = (FVector) obj;
        Object[] arr1 = data;
        Object[] arr2 = vec2.data;
        int n1 = size();
        int n2 = vec2.size();
        IntSequence inds1 = getIndexesForce();
        IntSequence inds2 = vec2.getIndexesForce();
        int n = n1 > n2 ? n2 : n1;
        for (int i = 0;  i < n;  i++) {
            Object v1 = arr1[inds1.intAt(i)];
            Object v2 = arr2[inds2.intAt(i)];
            if (v1 != v2)
                {int d = ((Comparable) v1).compareTo((Comparable) v2); if (d != 0)  return d; };
        }
        return n1 - n2;
    }

}
