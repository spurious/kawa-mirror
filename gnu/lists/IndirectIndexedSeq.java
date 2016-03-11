// Copyright (c) 2015  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.lists;
import java.util.List;

/** Wrap a List (or an indexed selection of it) as a Sequence. */

public class IndirectIndexedSeq<E>
    extends AbstractSequence<E> implements Sequence<E>, Array<E> {

    List<E> base;
    IntSequence indexes;

    public IndirectIndexedSeq(List<E> base, IntSequence indexes) {
        this.base = base;
        this.indexes = indexes;
    }

    @Override
    public int size() {
        return indexes.size();
    }

    public int getElementKind() {
        return (base instanceof AbstractSequence
                ? ((AbstractSequence) base).getElementKind()
                : null);
    }

    public int getBufferLength() { return base.size(); }

    @Override
    public int effectiveIndex(int i) { return indexes.getInt(i); }

    @Override
    public E get(int index) {
        return base.get(indexes.getInt(index));
    }
    @Override
    public E set(int index, E value) {
        return base.set(indexes.getInt(index), value);
    }

    @Override
    public E getRaw(int i) { return base.get(i); }

    @Override
    public void setRaw(int index, E value) { base.set(index, value);  }

    public void copyBuffer(int length) { throw unsupported("copyBuffer"); }
}
