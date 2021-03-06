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


    /** Makes a copy of (part of) the argument array. */
    public S16Vector(short[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final int getIntRaw(int index) {
        return (int) data[index];
    }

    public final Short get(int index) {
        return Short.valueOf(data[effectiveIndex(index)]);
    }

    public final Short getRaw(int index) {
        return Short.valueOf(data[index]);
    }

    @Override
    public final void setRaw(int index, Short value) {
        data[index] = value.shortValue();
    }

    @Override
    protected S16Vector newInstance(int newLength) {
        return new S16Vector(newLength < 0 ? data : new short[newLength]);
    }

    public int getElementKind() { return INT_S16_VALUE; }

    public String getTag() { return "s16"; }

    public int compareTo(Object obj) {
        return compareToInt(this, (S16Vector) obj);
    }

}
