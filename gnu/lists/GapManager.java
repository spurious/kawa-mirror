package gnu.lists;

import java.io.IOException;
import java.io.Writer;

/** An index mapper that implements a gap-buffer.
 * It is used as the {@code indexes} field of an {@code IndirectIndexable}
 * object.
 * A {@code GapManager} may not be shared: A {@code GapManager} can only be
 * the {@code indexes} field of one single {@code IndirectIndexable} object.
 * The combination of an {@code IndirectIndexable} and its {@code GapManager}
 * implement a "gap vector": array with a gap in the middle,
 * allowing efficient insert and delete
 * When viewed as an {@code IntSequence} then a {@code GapManager} can be
 * considered as the concatenation of two ranges: from 0 to the gapStart;
 * then from the gapEnd to the end of the data array.
 */

public class GapManager implements IntSequence {
    protected int gapStart;
    protected int gapEnd;
    protected int size;

    private GapManager() {
    }

    public GapManager(SimpleVector vector) {
        int sz = vector.size();
        size = sz;
        gapStart = sz;
        gapEnd = vector.getBufferLength();
    }

    public static GapManager getEmptyInstance(SimpleVector vector) {
        GapManager manager = new GapManager();
        manager.gapStart = 0;
        manager.gapEnd = vector.getBufferLength();
        manager.size = 0;
        return manager;
    }

    public int getGapStart() { return gapStart; }
    public int getGapEnd() { return gapEnd; }

    public int intAt(int index) {
        return index < gapStart ? index : index + (gapEnd-gapStart);
    }

    public IntSequence subList(int fromIx, int toIx) {
        IntSequence range = new Range.IntRange(fromIx, 1, toIx-fromIx);
        return new Sequences.ComposedIndexes(this, range);
    }

    public int size() { return size; }

    /** Adjust gap to 'where', and make sure it is least `needed' elements long. */
    protected void gapReserve(SimpleVector base, int where, int needed) {
        /* DEBUG
        int sz = size();
        Object[] values = new Object[sz];
        for (int i = 0;  i < sz;  i++)
            values[i] = base.get(i);
        */
        if (needed > gapEnd - gapStart) {
            // Need to resize.
            int oldLength = base.size(); // FIXME getBufferLength() ????
            int newLength = oldLength < 16 ? 16 : 2 * oldLength; // FIXME
            int size = oldLength - (gapEnd - gapStart);
            int minLength = size + needed;
            if (newLength < minLength)
                newLength = minLength;
            int newGapEnd = newLength - size + where;
            resizeShift(base, gapStart, gapEnd, where, newGapEnd);
            gapStart = where;
            gapEnd = newGapEnd;
        }
        else if (where != gapStart)
            shiftGap(base, where);

        /* DEBUG
        sz = size();
        if (sz != values.length)
            throw new Error("gapReserve error");
        for (int i = 0;  i < sz;  i++) {
            Object val = base.get(i);
            if (values[i] == val)
                continue;
            if (val == null || values[i] == null
                || ! (val.equals(values[i])))
                throw new Error("gapReserve error");
        }
        */
  }
    protected void insertUnspecified(SimpleVector base, int index, int count) {
        gapReserve(base, index, count);
        gapStart += count;
        size += count;
    }
    protected void delete(SimpleVector base, int start, int count) {
        shiftGap(base, start);
        gapEnd += count;
        size -= count;
    }

    protected void shiftGap(SimpleVector base, int newGapStart) {
        int delta = newGapStart - gapStart;
        if (delta > 0)
            base.shift(gapEnd, gapStart, delta);
        else if (delta < 0)
            base.shift(newGapStart, gapEnd + delta, - delta);
        gapEnd += delta;
        gapStart = newGapStart;
    }

    /** Used to grow and maybe move gap. */
    static void resizeShift(SimpleVector base,
                            int oldGapStart, int oldGapEnd,
                            int newGapStart, int newGapEnd) {
        base.checkCanWrite();
        int oldGapSize = oldGapEnd - oldGapStart;
        int newGapSize = newGapEnd - newGapStart;
        int oldLength = base.getBufferLength();
        int newLength = oldLength - oldGapSize + newGapSize;
        if (newLength > oldLength) {
            base.setBufferLength(newLength);
            //size = newLength; // ???
        }
        int gapDelta = oldGapStart - newGapStart;
        if (gapDelta >= 0) {
            int endLength = oldLength - oldGapEnd;
            base.shift(oldGapEnd, newLength - endLength, endLength);
            if (gapDelta > 0)
                base.shift(newGapStart, newGapEnd, gapDelta);
        } else {
            int endLength = newLength - newGapEnd;
            base.shift(oldLength-endLength, newGapEnd, endLength);
            base.shift(oldGapEnd, oldGapStart, newGapStart-oldGapStart);
        }
        base.clearBuffer(newGapStart, newGapSize);
    }

    protected void adjustPositions(int low, int high, int delta) {
        // Do nothing
    }

  /** If needed, move the gap so the given segment is contiguous.
   * @return the offset in the base array containing the segment,
   * or -1 if the parameters are bad.
   */
   
    protected int getSegment(SimpleVector<?> base, int where, int len,
                             boolean readOnly) {
        int length = base.size();
        if (where < 0 || where > length)
            return -1;
        if (len < 0)
            len = 0;
        else if (where + len > length)
            len = length - where;
        // if (len < 0 || where + len > length)
        //   return -1;
        if (where + len <= gapStart)
            return where;
        if (where >= gapStart)
            return where + (gapEnd - gapStart);
        if (readOnly)
            return -1;
        // Shift the gap depending in which direction needs least copying.
        if (gapStart - where > (len >> 1)) {
            shiftGap(base, where + len);
            return where;
        } else {
            shiftGap(base, where);
            return where + (gapEnd - gapStart);
        }
    }

    protected void writeTo(AbstractCharVector base, int start, int count,
                           Writer dest) throws IOException {
        char[] array = base.data;
        if (start < gapStart) {
            int count0 = gapStart - start;
            if (count0 > count)
                count0 = count;
            dest.write(array, start, count0);
            count -= count0;
            start += count;
        }
        if (count > 0) {
            start += gapEnd - gapStart;
            dest.write(array, start, count);
        }
    }

    public String toString() {
        return "GapManager[gap:("+gapStart+" "+gapEnd+") size:"+size+"]";
    }
}
