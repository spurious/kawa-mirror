// Copyright (c) 2001, 2002, 2003, 2008  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;
import java.util.*;

/** A generic simple vector.
 * <p>
 * This uses a simple data array along with an optional index mapper.
 * If {@code indexes} (the index mapper) is null, then {@code get(i)}
 * is {@code data[i]}.
 * Otherwise, {@code get(i)} is  {@code data[indexes.intAt(i)]}.
 * This allows general indirection and selection: Typically {@code indexes}
 * will be a {@code Range} but it can be any {@code IntSequence}.
 * <p>
 * If the sequence change size, then {@code indexes} is restricted
 * to either a {@code GapManager} or a {@code StableManager} (which
 * extends {@code GapManager}).  Those are usually created lazily: A plain
 * {@code GapManager} on the first insertion or deletion;
 * a {@code StableManager} if a Marker is requested.
 * <p>
 * If {@code indexes} is the special object {@code cantWriteMarker}
 * then this sequence is immutable.
 */

public abstract class SimpleVector<E> extends IndirectIndexable<E>
    implements Array<E>, Externalizable, RandomAccess
{
    protected GapManager getGapManager() {
        IntSequence ind = indexes;
        if (ind instanceof GapManager)
            return (GapManager) ind;
        if (ind != null) {
            checkCanWrite();
            String msg = indexes == cantWriteMarker ?
                "can't adjust size of constant vector" :
                "can't adjust size of indirect vector";
            throw new UnsupportedOperationException(msg);
        }
        GapManager manager = new GapManager(this);
        indexes = manager;
        return manager;
    }

    protected StableManager getStableManager() {
        IntSequence ind = indexes;
        if (ind instanceof StableManager)
            return (StableManager) ind;
        StableManager manager;
        if (ind instanceof GapManager) {
            manager = new StableManager(this, (GapManager) ind);
        } else if (ind != null) {
            checkCanWrite();
            String msg = indexes == cantWriteMarker ?
                "can't adjust size of constant vector" :
                "can't adjust size of indirect vector";
            throw new UnsupportedOperationException(msg);
        } else
            manager = new StableManager(this);
        indexes = manager;
        return manager;
    }

    protected abstract void setBuffer(Object obj);
    public abstract void setBufferLength(int length);

    /** Get sub-range of this vector, starting at given index.
     * @return {@code (size<<32)|where}
     *   such that {@code get(i)} is {@code data[where]};
     *   {@code get(i+1)} is {@code data[where+1]};
     *   until {@code get(i+size-1)}.
     *   The {@code size} is at least 1 (unless {@code index==size()}),
     *   but we try to do better.
     */
    public long getSegment(int index) {
        IntSequence ind = indexes;
        int sz = size();
        int where, size;
        Range.IntRange range;
        if (ind == null || ind == cantWriteMarker) {
            where = index;
            size = sz - where;
        } else if (ind instanceof Range.IntRange
                   && (range = (Range.IntRange) ind).getStepInt() == 1) {
            int istart = range.getStartInt();
            where = index + istart;
            size = ind.size() - index;
        } else if (ind instanceof GapManager) {
            GapManager gman = (GapManager) ind;
            int gapStart = gman.getGapStart();
            if (index < gapStart) {
                where = index;
                size = gapStart-index;
            } else {
                where = index + gman.getGapEnd() - gapStart;
                size = getBufferLength() - where;
            }
        } else {
            where = ind.intAt(index);
            size = index < sz ? 1 : 0;
        }
        return ((long) size << 32) | (long) where;
    }

    public int getSegment(int index, int len) {
        IntSequence ind = indexes;
        if (ind instanceof GapManager)
            return ((GapManager) ind).getSegment(this, index, len, false);
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
    // IndirectIndexable or to AbstractSequence?  FIXME

  protected boolean isAfterPos(int ipos) {
      return (ipos & 1) != 0;
  }

   protected abstract Object getBuffer();

    public E get(int index) {
        if (indexes != null)
            index = indexes.intAt(index);
        return getBuffer(index);
    }

  public E getRowMajor (int i)
  {
    return get(i);
  }

  protected abstract E getBuffer(int index);

    public E set(int index, E value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        if (indexes != null) {
            index = indexes.intAt(index);
        }
        E old = getBuffer(index);
        setBuffer(index, value);
        return old;
    }

    @Override
    public void setAt(int index, E value) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        if (indexes != null) {
            index = indexes.intAt(index);
        }
        setBuffer(index, value);
    }

    protected abstract void setBuffer(int index, E value);

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
    //             action.accept(getBuffer(where+i));
    //         len -= size;
    //         index += size;
    //     }
    // }
    /* #endif */

    public void fill(E value) {
        checkCanWrite();
        IntSequence ind = getIndexesForce();
        for (int i = size();  --i >= 0; )
            setBuffer(ind.intAt(i), value);
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
        setBuffer(index, o);
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
        getGapManager().insertUnspecified(this, index, count);
    }

    public void delete(int start, int end) {
        GapManager manager = getGapManager();
        int count = end-start;
        manager.delete(this, start, count);
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
        out.writeObject(indexes);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        setBuffer(in.readObject());
        indexes = (IntSequence) in.readObject();
    }

  public Array transpose(int[] lowBounds, int[] dimensions,
			 int offset0, int[] factors)
  {
    GeneralArray array = new GeneralArray();
    array.strides = factors;
    array.dimensions = dimensions;
    array.lowBounds = lowBounds;
    array.offset = offset0;
    array.base = this;
    array.simple = false;
    return array;
  }
}
