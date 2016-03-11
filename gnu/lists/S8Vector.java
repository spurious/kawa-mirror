// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 8-bit integers (bytes). */

public  class S8Vector extends ByteVector<Byte>
{
    public S8Vector() {
        data = empty;
    }

    public S8Vector(int size, byte value) {
        byte[] array = new byte[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public S8Vector(int size) {
        this(new byte[size]);
    }

    /** Reuses the argument without making a copy. */
    public S8Vector(byte[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public S8Vector(byte[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final int getIntRaw(int index) {
        return (int) data[index];
    }

    public final Byte get(int index) {
        return Byte.valueOf(data[effectiveIndex(index)]);
    }

    public final Byte getRaw(int index) {
        return Byte.valueOf(data[index]);
    }

    @Override
    public final void setRaw(int index, Byte value) {
        data[index] = value.byteValue();
    }

    @Override
    protected S8Vector newInstance(int newLength) {
        return new S8Vector(newLength < 0 ? data : new byte[newLength]);
    }

    public int getElementKind() { return INT_S8_VALUE; }

    public String getTag() { return "s8"; }

    public int compareTo(Object obj) {
        return compareToInt(this, (S8Vector) obj);
    }

}
