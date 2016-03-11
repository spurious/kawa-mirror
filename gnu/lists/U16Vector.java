// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;
import gnu.math.UShort;

/** Simple adjustable-length vector of unsigned 16-bit integers (shorts). */

public  class U16Vector extends ShortVector<UShort>
{
    public U16Vector() {
        data = empty;
    }

    public U16Vector(int size, short value) {
        short[] array = new short[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public U16Vector(int size) {
        this(new short[size]);
    }

    /** Reuses the argument without making a copy. */
    public U16Vector(short[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public U16Vector(short[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final int getIntRaw(int index) {
        return (int) data[index] & 0xffff;
    }

    public final UShort get(int index) {
        return UShort.valueOf(data[effectiveIndex(index)]);
    }

    public final UShort getRaw(int index) {
        return UShort.valueOf(data[index]);
    }

    @Override
    public final void setRaw(int index, UShort value) {
        data[index] = value.shortValue();
    }

    @Override
    protected U16Vector newInstance(int newLength) {
        return new U16Vector(newLength < 0 ? data : new short[newLength]);
    }

    public int getElementKind() { return INT_U16_VALUE; }

    public String getTag() { return "u16"; }

    public int compareTo(Object obj) {
        return compareToInt(this, (U16Vector) obj);
    }

}
