// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 16-bit integers (shorts). */

public  class S16Vector extends ShortVector<Short>
{
    public S16Vector() {
        data = empty;
    }

    public S16Vector(int size, short value) {
        short[] array = new short[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public S16Vector(int size) {
        this(new short[size]);
    }

    /** Reuses the argument without making a copy. */
    public S16Vector(short[] data) {
        this.data = data;
    }

    /*
    public S16Vector(Sequence seq) {
        data = new short[seq.size()];
        addAll(seq);
    }
    */

    public S16Vector(short[] data, IntSequence indexes) {
        this.data = data;
        this.indexes = indexes;
    }

    /** Makes a copy of (part of) the argument array. */
    public S16Vector(short[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final int intAtBuffer(int index) {
        return (int) data[index];
    }

    public final Short get(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return Short.valueOf(data[index]);
    }

    public final Short getBuffer(int index) {
        return Short.valueOf(data[index]);
    }

    @Override
    public final void setBuffer(int index, Short value) {
        data[index] = value.shortValue();
    }

    @Override
    protected S16Vector withIndexes(IntSequence ind) {
        return new S16Vector(data, ind);
    }

    @Override
    public S16Vector subList(int fromIx, int toIx) {
        return new S16Vector(data, indexesSubList(fromIx, toIx));
    }

    public int getElementKind() { return INT_S16_VALUE; }

    public String getTag() { return "s16"; }

    public int compareTo(Object obj) {
        return compareToInt(this, (S16Vector) obj);
    }

}
