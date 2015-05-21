package gnu.lists;

import gnu.kawa.functions.AddOp;
import gnu.kawa.functions.MultiplyOp;

public class Range<E> extends AbstractSequence implements Sequence {
    E start;
    Object step;
    int size;

    public Range(E start, Object step, int size) {
        this.start = start;
        this.step = step;
        this.size = size;
    }
    
    public E getStart() { return start; }
    public Object getStep() { return step; }

    @Override
    public E get (int index) {
        if (index >= size && size >= 0)
                throw new IndexOutOfBoundsException();
        return (E) AddOp.$Pl(start, MultiplyOp.apply(index, step));
    }
   
    @Override
    public int size() { return size; }

    @Override
    protected int nextIndex(int ipos) {
        return ipos == -1 ? size : ipos >>> 1;
    }

    public static class IntRange extends Range<Integer>
    implements IntSequence {
        int istart;
        int istep;

        public IntRange(int start, int step, int size) {
            super(start, step, size);
            this.istart = start;
            this.istep = step;
        }

        public IntRange(int start, int step) {
            super(start, step, -1);
            this.istart = start;
            this.istep = step;
        }

        public int getStartInt() { return istart; }

        public int intAt(int index) {
            if (index >= size && size >= 0)
                throw new IndexOutOfBoundsException();
            return istart + istep * index;
        }

        @Override
        public Integer getStart() { return getStartInt(); }

        @Override
        public Integer get(int index) { return intAt(index); }
    };

    //public static interface ByOne {
    //};
    public static void listAll(Range r) {
        int i = 0;
        for (Object x : r) {
            System.err.println("["+(i++)+"]: "+r);
        }
    }
}
