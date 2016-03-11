// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 32-bit integers (ints). */

public  class S32Vector extends IntVector<Integer>
    implements IntSequence
{
    public S32Vector() {
        data = AbstractSequence.noInts;
    }

    public S32Vector(int size, int value) {
        int[] array = new int[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public S32Vector(int size) {
        this(new int[size]);
    }

    /** Reuses the argument without making a copy. */
    public S32Vector(int[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public S32Vector(int[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final long getLongRaw(int index) {
        return (long) data[index];
    }

    public final Integer get(int index) {
        return Integer.valueOf(data[effectiveIndex(index)]);
    }

    public final Integer getRaw(int index) {
        return Integer.valueOf(data[index]);
    }

    @Override
    public final void setRaw(int index, Integer value) {
        data[index] = value.intValue();
    }

    @Override
    protected S32Vector newInstance(int newLength) {
        return new S32Vector(newLength < 0 ? data : new int[newLength]);
    }

    public int getElementKind() { return INT_S32_VALUE; }

    public String getTag() { return "s32"; }

    public int compareTo(Object obj) {
        return compareToInt(this, (S32Vector) obj);
    }

}
