package gnu.lists;

import gnu.mapping.Lazy;
import gnu.mapping.Promise;
import java.util.List;

/** Indexing "composes" with a set of indexing arrays. */

public class ComposedArray<E> extends TransformedArray<E> {

    Array<Integer>[] mappers;
    int rank;
    int[] dims;
    int[] lowBounds;

    public ComposedArray(Array base, Array<Integer>... mappers) {
        super(base);
        this.mappers = mappers;
        setup();
    }

    @Override public int rank() { return rank; }

    @Override
    public int getLowBound(int dim) { return lowBounds[dim]; }

    @Override
    public int getSize(int dim) { return dims[dim]; }

    static int rankSum(Array<?>[] mappers) {
        int rank = 0;
        for (int i = mappers.length;  --i >= 0; )
            rank += mappers[i].rank();
        return rank;
    }

    void setup() {
        int k = 0;
        int n = mappers.length;
        rank = rankSum(mappers);
        dims = new int[rank];
        lowBounds = new int[rank];
        for (int i = 0; i < n; i++) {
            Array<Integer> mapper = mappers[i];
            int mrank = mapper.rank();
            for (int j = 0;  j < mrank;  j++) {
                dims[k] = mapper.getSize(j);
                lowBounds[k] = mapper.getLowBound(j);
                k++;
            }
        }
    }

    @Override
    public final int effectiveIndex() {
        return resolve(0, 0, 0, noInts);
    }
    public final int effectiveIndex(int i) {
        return resolve(i, 0, 0, noInts);
    }
    public final int effectiveIndex(int i, int j) {
        return resolve(i, j, 0, noInts);
    }
    public final int effectiveIndex(int i, int j, int k, int... rest) {
        return resolve(i, j, k, rest);
    }

    private int resolve(int x, int y, int z, int... rest) {
        int brank = base.rank(); // must be same as mappers.length
        int rlen = rest.length;
            // The output arguments (which will be used to index into base)
            // are in the "virtual array" [b0 b1 b2 @brest]
            int b0 = 0, b1 = 0, b2 = 0;
            int[] brest = brank > 3 ? new int[brank-3] : noInts;
            // bpos is the next unwritten position in [b0 b1 b2 @brest]
            int bpos = 0;
            // mpos is the index of next available value in [x y z @rest]
            int mpos = 0;
            for (int i = 0; i < brank;  i++) {
                Array<Integer> mapper = mappers[i];
                int mrank = mapper.rank();
                int bval;
                // The current value of x is mpos index in the original value
                // of  [x y z @rest].  Similarly for y at mpos+1,
                // and z at mpos+2.
                switch (mrank) {
                case 0:
                    bval = mapper.getInt();
                    break;
                case 1:
                    bval = mapper.getInt(x);
                    x = y;
                    y = z;
                    if (mpos >= 3 && mpos-3 < rlen)
                        z = rest[mpos-3];
                    break;
                case 2:
                    bval = mapper.getInt(x, y);
                    x = z;
                    if (mpos >= 3 && mpos-3 < rlen) {
                        y = rest[mpos-3];
                        if (mpos >= 2 && mpos-2 < rlen)
                            z = rest[mpos-2];
                    }
                    break;
                default:
                    int[] tmp;
                    if (mrank == 3)
                        tmp = noInts;
                    else {
                        tmp = new int[mrank-3];
                        System.arraycopy(rest, mpos-3, tmp, 0, mrank-3);
                    }
                    bval = mapper.getInt(x, y, z, tmp);
                    if (mpos >= 3 && mpos-3 < rlen) {
                        x = rest[mpos-3];
                        if (mpos >= 2 && mpos-2 < rlen)
                            y = rest[mpos-2];
                        if (mpos >= 1 && mpos-1 < rlen)
                            z = rest[mpos-1];
                    }
                    
                }
                mpos += mrank;
                switch (bpos) {
                case 0:  b0 = bval;  break;
                case 1:  b1 = bval;  break;
                case 2:  b2 = bval;  break;
                default: brest[bpos-3] = bval;  break;
                }
                bpos++;
            }
            switch (bpos) {
            case 0:  return base.effectiveIndex();
            case 1:  return base.effectiveIndex(b0);
            case 2:  return base.effectiveIndex(b0, b1);
            default: return base.effectiveIndex(b0, b1, b2, brest);
            }
        }


    public static Object generalIndex(Array arr, boolean shared,
                                      Object... indexes) {
        return generalIndex(arr, shared, 0, indexes.length, indexes);
    }
    public static Object generalIndex(Array arr, boolean shared,
                                      int start, int nindexes,
                                      Object[] indexes) {
        boolean allInts = true;
        for (int i = 0;  i < nindexes; i++) {
            Object index = indexes[start+i];
            if (index instanceof Number) {
                // simple
            } else if (index instanceof Lazy) {
                index = Promise.force(index);
                indexes[start+i] = index;
                if (! (index instanceof Number))
                    allInts = false;
            } else {
                allInts = false;
            }
        }
        boolean linear = true;
        if (allInts && ! shared) {
            switch (nindexes) {
            case 0:
                return arr.get();
            case 1:
                return arr.get(((Number) indexes[start]).intValue());
            case 2:
                return arr.get(((Number) indexes[start]).intValue(),
                               ((Number) indexes[start+1]).intValue());
                
            default:
                int[] rest = nindexes == 3 ? AbstractSequence.noInts
                    : new int[nindexes];
                for (int i = 3;  i < nindexes; i++)
                    rest[i-3] = ((Number) indexes[start+i]).intValue();
                return arr.get(((Number) indexes[start]).intValue(),
                               ((Number) indexes[start+1]).intValue(),
                               ((Number) indexes[start+2]).intValue(),
                               rest);
            }
        } else {
            Array<Integer>[] aindexes = (Array<Integer>[]) new Array[nindexes];
            int rank = 0;
            for(int i = 0;  i < nindexes;  i++) {
                Array<Integer> iarr = Arrays.asIntArrayOrNull(indexes[start+i]);
                if (iarr == null)
                    throw new ClassCastException("index is not an integer or integer array "+indexes[start+i].getClass().getName());
                aindexes[i] = iarr;
                if (! (iarr.rank() == 0
                       || iarr instanceof Range.IntRange))
                    linear = false;
                rank += iarr.rank();
            }
            if (! shared && arr instanceof SimpleVector) {
                if (linear && rank == 1 && nindexes == 1) {
                    SimpleVector svec = (SimpleVector) arr;
                    Range.IntRange range = (Range.IntRange) aindexes[0];
                    return Sequences.copy(svec, range, false);
                }
                arr = ((SimpleVector) arr).asImmutable();
                shared = true;
            }
            // FIXME: replace 'shared' by '(shared || arr.isImmutable())' ?
            if (linear && shared && arr instanceof GeneralArray) {
                GeneralArray garr = (GeneralArray) arr;
                int[] dimensions = new int[rank];
                int[] lowBounds = new int[rank];
                int offset = garr.offset;
                int[] strides = new int[rank];
                int k = 0;
                for(int i = 0;  i < nindexes;  i++) {
                    Array<Integer> iarr = aindexes[i];
                    int irank = iarr.rank();
                    int istride = garr.strides[i];
                    int ilow = garr.lowBounds[i];
                    int idim = garr.dimensions[i];
                    if (iarr.rank() == 0) {
                        int j = iarr.getInt();
                        j -= ilow;
                        if (j < 0 || j >= idim) {
                            throwBoundException(i, nindexes, garr,
                                                "value "+(j+ilow));
                        }
                        offset += istride * j;
                    } else { // iarr instanceof Range.IntRange
                        Range.IntRange irange = (Range.IntRange) iarr;
                        int sz = irange.size();
                        int j = irange.getStartInt();
                        j -= ilow;
                        if (j < 0 || j > idim
                            || (j >= 0 && j+sz > idim)) {
                            StringBuilder sbuf = new StringBuilder();
                            sbuf.append("range [");
                            sbuf.append(j+ilow);
                            if (sz < 0)
                                sbuf.append(" <:]");
                            else {
                                sbuf.append(" <: ");
                                sbuf.append(sz+j+ilow);
                                    sbuf.append("]");
                            }
                            throwBoundException(i, nindexes, garr,
                                                sbuf.toString());
                        }
                        if (sz < 0)
                            sz = idim - j;
                        dimensions[k] = sz;
                        lowBounds[k] = 0;
                        strides[k] = irange.getStepInt() * istride;
                        offset += istride * j;
                        k++;
                    }
                }
                return GeneralArray.make(garr.getBase(), dimensions, lowBounds,
                                         strides, offset);
             }
            ComposedArray carr = new ComposedArray(arr, aindexes);
            if (! shared) {
                return Arrays.simpleCopy(carr, false);
            }
            else if (carr.rank() == 1 && carr.getLowBound(0) == 0) {
                if (arr instanceof List && aindexes.length == 1
                    && aindexes[0] instanceof IntSequence)
                    return new IndirectIndexedSeq((List) arr,
                                                  (IntSequence) aindexes[0]);
                return new ComposedArray.AsSequence(carr);
            }
            else {
                return carr;
            }
        }
    }

   private static void throwBoundException(int i, int nindexes,
                                           GeneralArray oldMapper,
                                           String index) {
        int ilow = oldMapper.lowBounds[i];
        int idim = oldMapper.dimensions[i];
        StringBuilder sbuf = new StringBuilder();
        if (nindexes == 0)
            sbuf.append("index " );
        else {
            sbuf.append("index (");
            sbuf.append(i);
            sbuf.append(" of ");
            sbuf.append(nindexes);
            sbuf.append(") ");
        }
        sbuf.append(index);
        sbuf.append(" out of bounds [");
        sbuf.append(ilow);
        sbuf.append(" <: ");
        sbuf.append(idim+ilow);
        sbuf.append("]");
        throw new IndexOutOfBoundsException(sbuf.toString());
    }

    public static class AsSequence<E>
            extends ComposedArray<E> implements AVector<E> {
        public AsSequence(ComposedArray old) {
            super(old.base, old.mappers);
            setup();
        }
    }
}
