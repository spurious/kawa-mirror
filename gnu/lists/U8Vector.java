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

    /*
    public U8Vector(Sequence seq) {
        data = new byte[seq.size()];
        addAll(seq);
    }
    */

    public U8Vector(byte[] data, IntSequence indexes) {
        this.data = data;
        this.indexes = indexes;
    }

    /** Makes a copy of (part of) the argument array. */
    public U8Vector(byte[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final int intAtBuffer(int index) {
        return (int) data[index] & 0xff;
    }

    public final UByte get(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return UByte.valueOf(data[index]);
    }

    public final UByte getBuffer(int index) {
        return UByte.valueOf(data[index]);
    }

    @Override
    public final void setBuffer(int index, UByte value) {
        data[index] = value.byteValue();
    }

    public int getElementKind() { return INT_U8_VALUE; }

    public String getTag() { return "u8"; }

    public int compareTo(Object obj) {
        return compareToInt(this, (U8Vector) obj);
    }

}
