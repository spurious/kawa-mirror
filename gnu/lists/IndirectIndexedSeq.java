// Copyright (c) 2015  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.lists;
import java.util.List;

/** Wrap a List (or an indexed selection of it) as a Sequence. */

public class IndirectIndexedSeq<E>
    extends IndirectIndexable<E> {

    List<E> base;

    public IndirectIndexedSeq(List<E> base, IntSequence indexes) {
        this.base = base;
        this.indexes = indexes;
        new Error("new IndirectIndexedSeq").printStackTrace();
    }

    public int getBufferLength() { return base.size(); }

    public E get(int index) {
        return base.get(indexes.intAt(index));
    }

    @Override
    protected IndirectIndexedSeq<E> withIndexes(IntSequence ind) {
        return new IndirectIndexedSeq<E>(base, ind);
    }
}
