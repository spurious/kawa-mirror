// Copyright (c) 2001, 2002, 2003, 2008, 2016  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;
import java.util.*;

/** A generic simple vector.
 */

public abstract class SimpleVector<E> extends AbstractSequence<E>
    implements AVector<E>, Externalizable, RandomAccess
{
    // 1-bit sign; 25-bit offset; 6-bit flags; 32-bit size;
    protected long info = 0x8000000000000000L;
    public static final int MAX_GAP_SIZE = (1 << 25) - 1;
    /** If isSimple(), the values are all the values of the guffer.
     * In this case getSize() == getBufferLength(); */
    protected final boolean isVerySimple() { return info < 0; }
    /** The values are buffer[offset <: offset+size]. */
    protected final boolean isSubRange() {
        return (info & SUBRANGE_FLAG) != 0; }
    /** The values are buffer[0 <: size] ++ buffer[gapEnd <: ],
     * where gapEnd = size + offset */
    protected final boolean isGapBuffer() {
        return (info & GAP_FLAG) != 0; }
    protected final void setInfoField(int size, int offset, long flags) {
        info = (0xFFFFFFFFL & (long) size) | ((long) offset << 38) | flags;
    }
    protected final int getGapStart() { return getSizeBits(); }
    protected final int getGapEnd() { return getSizeBits()+getOffsetBits(); }
    protected final void setGapBounds(int gapStart, int gapEnd, long flags) {
        setInfoField(gapStart, gapEnd-gapStart, flags|GAP_FLAG);
    }
    protected final void setGapBounds(int gapStart, int gapEnd) {
        setInfoField(gapStart, gapEnd-gapStart, (info & 0x3F00000000l) | GAP_FLAG);
    }
    protected final int getSizeBits() { return (int) info; }
    protected final int getOffsetBits() { return (int) (info >> 38); }
    protected static final long READ_ONLY_FLAG = 0x100000000L;
    protected static final long SHARED_FLAG = 0x200000000L;
    protected static final long COPY_ON_WRITE = 0x400000000L;
    // Could define GAP_FLAG in terms ! SUBRANGE_FLAG && !isVerySimple()
    protected static final long SUBRANGE_FLAG = 0x1000000000L;
    protected static final long GAP_FLAG = 0x2000000000L;

    public int size() {
        int len = getBufferLength();
        if (isVerySimple())
            return len;
        if ((info & SUBRANGE_FLAG) != 0)
            return getSizeBits();
        else // gap
            return len - getOffsetBits();
    }

    public int effectiveIndex(int index) {
        if (isVerySimple())
            return index;
        if ((info & SUBRANGE_FLAG) != 0) {
            // offset is start index
            if (index >= getSizeBits())
                throw new IndexOutOfBoundsException();
            return index + getOffsetBits();
        } else {
            // gapStart is 32-bit size "size"; gapSize is 24-bit offset
            if (index >= getSizeBits())
                index += getOffsetBits();
            return index;
        }
    }

    protected void gapReserve(int where, int needed) {
        gapReserveGeneric(where, needed);
    }
    protected final void gapReserveGeneric(int where, int needed) {
        if ((info & (READ_ONLY_FLAG|SHARED_FLAG)) != 0) {
            String msg = (info & (READ_ONLY_FLAG)) != 0 ?
                "can't adjust size of constant vector" :
                "can't adjust size of indirect vector";
            throw new UnsupportedOperationException(msg+" info:"+Long.toHexString(info));
        }
        int sz = size();
        int blen = getBufferLength();
        if ((info & (COPY_ON_WRITE)) != 0) // FIXME handled by checkCanWrite ?
            doCopyOnWrite(size()); // FIXME does needless moves
        if (isVerySimple()) {
            setGapBounds(sz, sz);
        } else if ((info & SUBRANGE_FLAG) != 0) {
            // FIXME
        }
        int gapStart = getSizeBits();
        int gapSize = getOffsetBits();
        int gapEnd = gapStart + gapSize;
        if (needed > gapEnd - gapStart) {
            // Need to resize.
            int oldLength = getBufferLength();
            int newLength = oldLength < 16 ? 16 : 2 * oldLength; // FIXME
            int size = oldLength - (gapEnd - gapStart);
            int minLength = size + needed;
            if (newLength < minLength)
                newLength = minLength;
            int newGapEnd = newLength - size + where;
            resizeShift(gapStart, gapEnd, where, newGapEnd);
            setGapBounds(where, newGapEnd);
        }
        else if (where != gapStart) {
            int delta = where - gapStart;
            if (delta > 0)
                shift(gapEnd, gapStart, delta);
            else if (delta < 0)
                shift(where, gapEnd + delta, - delta);
            else
                return;
            setGapBounds(where, gapEnd+delta);
        }
    }

    /** Used to grow and maybe move gap. */
    void resizeShift(int oldGapStart, int oldGapEnd,
                            int newGapStart, int newGapEnd) {
        //base.checkCanWrite();
        int oldGapSize = oldGapEnd - oldGapStart;
        int newGapSize = newGapEnd - newGapStart;
        int oldLength = getBufferLength();
        int newLength = oldLength - oldGapSize + newGapSize;
        if (newLength > oldLength) {
            copyBuffer(newLength);
            //size = newLength; // ???
        }
        int gapDelta = oldGapStart - newGapStart;
        if (gapDelta >= 0) {
            int endLength = oldLength - oldGapEnd;
            shift(oldGapEnd, newLength - endLength, endLength);
            if (gapDelta > 0)
                shift(newGapStart, newGapEnd, gapDelta);
        } else {
            int endLength = newLength - newGapEnd;
            shift(oldLength-endLength, newGapEnd, endLength);
            shift(oldGapEnd, oldGapStart, newGapStart-oldGapStart);
        }
        clearBuffer(newGapStart, newGapSize);
    }

    protected abstract void setBuffer(Object obj);
    public abstract int getBufferLength();
    public abstract void copyBuffer(int length);
    protected abstract SimpleVector newInstance(int newSize);

    public SimpleVector<E> asImmutable() {
        if ((info & READ_ONLY_FLAG) != 0)
            return this;
        if (isVerySimple()) {
            SimpleVector<E> tmp = newInstance(-1);
            this.info |= COPY_ON_WRITE;
            tmp.info |= READ_ONLY_FLAG;
            return tmp;
        }
        return Arrays.flattenCopy(this, false);
    }

    protected void checkCanWrite () {
        long fl = info;
        if ((fl & COPY_ON_WRITE) != 0) {
            doCopyOnWrite(size());
        }
        if ((fl & READ_ONLY_FLAG) != 0)
            throw new UnsupportedOperationException("write not allowed to read-only "+(rank()==1 ? "sequence" : "array"));
    }

    protected void doCopyOnWrite(int sz) {
        long fl = info;
        Object old = getBuffer();
        // FIXME inefficient copy
        copyBuffer(sz);
        if ((fl & SUBRANGE_FLAG) != 0) {
            System.arraycopy(old, getOffsetBits(),
                             getBuffer(), 0, sz);
            info = -1;
        }
        fl &= ~COPY_ON_WRITE;
        info = fl;
    }

    /** Get sub-range of this vector, starting at given index.
     * @return {@code (size<<32)|where}
     *   such that {@code get(i)} is {@code data[where]};
     *   {@code get(i+1)} is {@code data[where+1]};
     *   until {@code get(i+size-1)}.
     *   The {@code size} is at least 1 (unless {@code index==size()}),
     *   but we try to do better.
     */
    public long getSegment(int index) {
        int sz = size();
        int where, size;
        if (isVerySimple()) {
            where = index;
            size = sz - index;
        } else if ((info & SUBRANGE_FLAG) != 0) {
            // values are buffer[offset <: offset+size]
            int istart = getOffsetBits();
            where = istart + index;
            size = sz - index;
        } else {
            int gapStart = getGapStart();
            int gEnd = getGapEnd();
            if (index < gapStart) {
                where = index;
                size = gapStart - index;
            } else {
                where = index + getGapEnd() - gapStart;
                size = getBufferLength() - where;
            }
        }
        return ((long) size << 32) | (long) where;
    }

    public int getSegment(int index, int len) {
        if (isGapBuffer()) {
            int sz = size();
            if (index < 0 || index > sz)
                return -1;
            if (index < 0)
                index = 0;
            else if (index + len > sz)
                len = sz - index;
            // if (len < 0 || index + len > size)
            //   return -1;
            int gapStart = getGapStart();
            if (index + len <= gapStart)
                return index;
            if (index >= gapStart)
                return index + (getGapEnd() - gapStart);
            if ((info & READ_ONLY_FLAG) != 0)
                return -1;
            // Shift the gap depending in which direction needs least copying.
            if (gapStart - index > (len >> 1)) {
                gapReserve(index + len, 0);
                return index;
            } else {
                gapReserve(index, 0);
                return index + (getGapEnd() - gapStart);
            }
        }
        return getSegmentReadOnly(index, len);
    }

    public int getSegmentReadOnly(int start, int len) {
        int sz = size();
        if (start < 0 || len < 0 || start + len > sz)
            return -1;
        long result = getSegment(start);
        int where = (int) result;
        int size = (int) (result >> 32);
        return size >= len ? where : -1;
    }

    // Should isAfterPos, nextPos, getPosNext ... be moved to
    // AbstractSequence?  FIXME

  protected boolean isAfterPos(int ipos) {
      return (ipos & 1) != 0;
  }

   protected abstract Object getBuffer();

  public E getRowMajor (int i)
  {
    return get(i);
  }

    /* #ifdef JAVA8 */
    // @Override
    // public void forEach(java.util.function.Consumer<? super E> action) {
    //     int len = size();
    //     int index = 0;
    //     while (len > 0) {
    //         long result = getSegment(index);
    //         int where = (int) result;
    //         int size = (int) (result >> 32);
    //         for (int i = 0; i < size; i++)
    //             action.accept(getRaw(where+i));
    //         len -= size;
    //         index += size;
    //     }
    // }
    /* #endif */

    public void fill(E value) {
        checkCanWrite();
        for (int i = size();  --i >= 0; )
            setRaw(effectiveIndex(i), value);
    }

  public void shift(int srcStart, int dstStart, int count)
  {
    checkCanWrite();
    Object data = getBuffer();
    System.arraycopy(data, srcStart, data, dstStart, count);
  }

    @Override
    public boolean add(E o) {
        add(size(), o);
        return true;
    }

    @Override
    public void add(int index, E o) {
        addSpace(index, 1);
        setRaw(index, o);
    }

    protected int addPos(int ipos, E value) {
        int index = nextIndex(ipos);
        add(index, value);
        // FIXME should use a 'adjustPos' method
        int ret = createPos(index+1, true);
        releasePos(ipos);
        return ret;
    }

    /** Insert count unspecified elements at index. */
    protected void addSpace(int index, int count) {
        gapReserve(index, count);
        setGapBounds(getGapStart() + count, getGapEnd());
    }

    public void delete(int start, int end) {
        gapReserve(start, 0);
        int gapStart = getSizeBits();
        int gapSize = getOffsetBits();
        int gapEnd = gapStart + gapSize;
        int count = end-start;
        setGapBounds(start, gapEnd + count);
        clearBuffer(start, count);
    }

    protected abstract void clearBuffer(int start, int count);

    public Object toDataArray() {
        Object buffer = getBuffer();
        Class componentType = buffer.getClass().getComponentType();
        int count = size();
        int index = 0;
        Object copy = java.lang.reflect.Array.newInstance(componentType, count);
        while (count > 0) {
            long result = getSegment(index);
            int where = (int) result;
            int size = (int) (result >> 32);
            if (size > count)
                size = count;
            System.arraycopy(buffer, where, copy, index, size);
            index += size;
            count -= size;
        }
        return copy;
    }

    /** This is convenience hack for printing "uniform vectors" (srfi 4).
     * It may go away without notice! */
    public String getTag() { return null; }

    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(getBuffer());
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        setBuffer(in.readObject());
    }
}
