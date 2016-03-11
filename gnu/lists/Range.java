package gnu.lists;

import gnu.kawa.functions.AddOp;
import gnu.kawa.functions.MultiplyOp;
import gnu.math.IntNum;
import java.io.*;

public class Range<E> extends AbstractSequence<E> implements Sequence<E> {
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
    public E getRaw(int index) { return get(index); }

    @Override
    public int size() { return size; }

    boolean isUnbounded() { return size == -1; }

    public static class IntRange extends Range<Integer>
        implements IntSequence, Externalizable {
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
        public int getStepInt() { return istep; }

        public int getInt(int index) {
            if (index >= size && size >= 0)
                throw new IndexOutOfBoundsException("index:"+index+" size:"+size);
            return istart + istep * index;
        }

        public IntRange subListFromRange(int rstart, int rstep, int rsize) {
            int nstart = istart + rstart * istep;
            int nstep = istep * rstep;
            if (isUnbounded() && rsize == -1)
                return new IntRange(nstart, nstep);
            int nsize;
            if (isUnbounded())
                nsize = rsize;
            else {
                nsize = (size-rstart+rstep-1) / rstep;
                if (rsize != -1) {
                    if (rsize > nsize)
                        throw new IndexOutOfBoundsException();
                    nsize = rsize;
                }
            }
            return new IntRange(nstart, nstep, nsize);
        }

        public IntRange subList(int fromIx, int toIx) {
            return subListFromRange(fromIx, 1, toIx - fromIx);
        }

        @Override
        public Integer getStart() { return getStartInt(); }

        @Override
        public Integer get(int index) { return getInt(index); }

        @Override
        public Integer getRaw(int index) { return getInt(index); }

        @Override
        public int getIntRaw(int index) { return getInt(index); }

        public int getElementKind() { return INT_S32_VALUE; }

        public void writeExternal(ObjectOutput out) throws IOException {
            out.writeInt(size);
            out.writeInt(istart);
            out.writeInt(istep);
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            size = in.readInt();
            istart = in.readInt();
            istep = in.readInt();
        }
    };

    //public static interface ByOne {
    //};
    public static void listAll(Range r) {
        int i = 0;
        for (Object x : r) {
            System.err.println("["+(i++)+"]: "+r);
        }
    }

    public String toString() {
        return "#<range start:"+getStart()+" step:"+getStep()+" size:"+size+">";
    }

    public static final IntRange zeroAndUp = new IntRange(0, 1);

    public static Range<?> valueOfUnbounded(Object start) {
        IntNum iistart = IntNum.asIntNumOrNull(start);
        if (iistart != null && iistart.inIntRange()) {
             int istart = iistart.intValue();
             return new IntRange(istart, 1);
        }
        return new Range<Object>(start, IntNum.one(), -1);
    }

    public static Range<?> valueOfLT(Object start, Object end) {
        IntNum iistart = IntNum.asIntNumOrNull(start);
        IntNum iiend = IntNum.asIntNumOrNull(end);
        if (iistart != null && iiend != null
            && iistart.inIntRange() && iiend.inIntRange()) {
            int istart = iistart.intValue();
            int iend = iiend.intValue();
            if (iend >= istart)
                return new IntRange(istart, 1, iend-istart);
        } else {
            int size = ((Number) AddOp.$Mn(end, start)).intValue();
            if (size >= 0)
                return new Range<Object>(start, IntNum.one(), size);
        }
        throw new IndexOutOfBoundsException("start index "+start+" is greater than end index "+end);
    }

    public static Range<?> valueOfLE(Object start, Object end) {
        IntNum iistart = IntNum.asIntNumOrNull(start);
        IntNum iiend = IntNum.asIntNumOrNull(end);
        if (iistart != null && iiend != null
            && iistart.inIntRange() && iiend.inIntRange()
            && iiend.intValue() != Integer.MAX_VALUE) {
            int istart = iistart.intValue();
            int iend = iiend.intValue() + 1;
            if (iend >= istart)
                return new IntRange(istart, 1, iend-istart);
        } else {
            int size = ((Number) AddOp.$Mn(end, start)).intValue() + 1;
            if (size >= 0)
                return new Range<Object>(start, IntNum.one(), size);
        }
        throw new IndexOutOfBoundsException("size (end-start+1 or "+end+"-"+start+"+1) is negative");
    }

    public static Range<?> valueOfGT(Object start, Object end) {
        if (start instanceof Integer && end instanceof Integer) {
            int istart = (Integer) start;
            int iend = (Integer) end;
            if (iend <= istart)
                return new IntRange(istart, -1, iend-istart);
        } else {
            int size = ((Number) AddOp.$Mn(start, end)).intValue();
            if (size >= 0)
                return new Range<Object>(start, IntNum.minusOne(), size);
        }
        throw new IndexOutOfBoundsException("start index "+start+" is less than end index "+end);
    }

    public static Range<?> valueOfGE(Object start, Object end) {
        if (start instanceof Integer && end instanceof Integer) {
            int istart = (Integer) start;
            int iend = (Integer) end;
            if (iend <= istart)
                return new IntRange(istart, -1, iend-istart+1);
        } else {
            int size = ((Number) AddOp.$Mn(start, end)).intValue() + 1;
            if (size >= 0)
                return new Range<Object>(start, IntNum.minusOne(), size);
        }
        throw new IndexOutOfBoundsException("size (start-end+1 or "+start+"-"+end+"+1) is negative");
    }
}
