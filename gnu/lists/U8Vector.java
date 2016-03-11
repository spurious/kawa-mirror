// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;
import gnu.math.UByte;

/** Simple adjustable-length vector of unsigned 8-bit integers (bytes). */

public  class U8Vector extends ByteVector<UByte>
{
    public U8Vector() {
        data = empty;
    }

    public U8Vector(int size, byte value) {
        byte[] array = new byte[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public U8Vector(int size) {
        this(new byte[size]);
    }

    /** Reuses the argument without making a copy. */
    public U8Vector(byte[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public U8Vector(byte[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final int getIntRaw(int index) {
        return (int) data[index] & 0xff;
    }

    public final UByte get(int index) {
        return UByte.valueOf(data[effectiveIndex(index)]);
    }

    public final UByte getRaw(int index) {
        return UByte.valueOf(data[index]);
    }

    @Override
    public final void setRaw(int index, UByte value) {
        data[index] = value.byteValue();
    }

    @Override
    protected U8Vector newInstance(int newLength) {
        return new U8Vector(newLength < 0 ? data : new byte[newLength]);
    }

    public int getElementKind() { return INT_U8_VALUE; }

    public String getTag() { return "u8"; }

    public int compareTo(Object obj) {
        return compareToInt(this, (U8Vector) obj);
    }

}
