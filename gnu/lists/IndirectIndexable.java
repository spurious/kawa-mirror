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

    public int size() { return indexes.size(); }

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
