// Copyright (c) 2015  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.lists;
import java.util.List;

public class IndirectIndexedSeq<E>
    extends AbstractSequence<E> implements Sequence<E> {

    List<E> base;
    IntSequence indexes;

    public IndirectIndexedSeq(List<E> base, IntSequence indexes) {
        this.base = base;
        this.indexes = indexes;
    }

    public E get(int index) {
        return base.get(indexes.intAt(index));
    }

    public int size() { return indexes.size(); }
}
