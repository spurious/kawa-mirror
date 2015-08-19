// Copyright (c) 2015  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.lists;
import java.util.List;

/** A sequence that uses a separate index mapper.
 * See SimpleVector for the most common usage.
 */

public abstract class IndirectIndexable<E>
    extends AbstractSequence<E> implements Sequence<E>
{
    protected IntSequence indexes;

    public int size() {
        return indexes == null || indexes == cantWriteMarker
            // or generally indexes.isUnbounded() ??? FIXME
            ? getBufferLength()
            : indexes.size();
    }

    public abstract int getBufferLength();

    public static final Range.IntRange cantWriteMarker =
        Range.IntRange.cantWriteMarker;

    protected void checkCanWrite () {
        if (indexes == cantWriteMarker)
            throw new UnsupportedOperationException();
    }
    protected IntSequence getIndexesForce() {
        // cantWriteMarker doubles as [0 .. infinity]
        return indexes == null ? cantWriteMarker : indexes;
    }

    /** Create a new instance with the same data and replaces indexes. */
    protected abstract IndirectIndexable<E> withIndexes(IntSequence ind);

    protected IntSequence indexesSelect(IntSequence selected) {
        IntSequence ind = indexes == null || indexes == cantWriteMarker
            ? new Range.IntRange(0, 1, getBufferLength())
            : indexes;
        return Sequences.indirectIndexed(ind, selected);
    }

    protected IntSequence indexesSubList(int fromIx, int toIx) {
        if (indexes == null || indexes == cantWriteMarker)
            return new Range.IntRange(fromIx, 1, toIx-fromIx);
        else
            return indexes.subList(fromIx, toIx);
    }

    public IndirectIndexable<E> select(IntSequence indexes) {
        return withIndexes(indexesSelect(indexes));
    }

    public IndirectIndexable<E> subList(int fromIx, int toIx) {
        return withIndexes(indexesSubList(fromIx, toIx));
    }

    protected boolean isAfterPos(int ipos) {
        return indexes != cantWriteMarker &&indexes instanceof AbstractSequence
            ? ((AbstractSequence) indexes).isAfterPos(ipos)
            : super.isAfterPos(ipos);
    }
    public int nextPos(int ipos) {
        return indexes != cantWriteMarker && indexes instanceof AbstractSequence
            ? ((AbstractSequence) indexes).nextPos(ipos)
            : super.nextPos(ipos);
    }
}
