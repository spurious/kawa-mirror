package gnu.lists;

public class Arrays {

    public static int rowMajorToEffectiveIndex(Array arr, int index) {
        if (arr instanceof GeneralArray) {
            GeneralArray garr = (GeneralArray) arr;
            if (garr.simple)
                return garr.base.effectiveIndex(index);
        }
        int r = arr.rank();
        switch (r) {
        case 0:
            return arr.effectiveIndex();
        case 1:
            return arr.effectiveIndex(index + arr.getLowBound(0));
        case 2:
            int sz1 = arr.getSize(1);
            return arr.effectiveIndex((index / sz1) + arr.getLowBound(0),
                                      (index % sz1) + arr.getLowBound(1));
        default:
            int[] rest = r == 3 ?  AbstractSequence.noInts : new int[r-3];
            int p = 1;
            int sz;
            for (int i = r;  --i >= 3; ) {
                sz = arr.getSize(i);
                rest[i] = (index % sz) + arr.getLowBound(i);
                index = index / sz;
            }
            sz = arr.getSize(2);
            int i2 = (index % sz) + arr.getLowBound(2);
            index = index / sz;
            sz = arr.getSize(1);
            int i1 = (index % sz) + arr.getLowBound(1);
            index = index / sz;
            sz = arr.getSize(0);
            int i0 = (index % sz) + arr.getLowBound(0);
            return arr.effectiveIndex(i0, i1, i2, rest);
        }
    }

    public static <E> E getRowMajor(Array<E> arr, int index) {
        return (E) arr.getRaw(rowMajorToEffectiveIndex(arr, index));
    }

    public static int hashCode(Array arr) {
        int rank = arr.rank();
        int[] indexes = new int[rank];
        int size = 1;
        for (int r = 0; r < rank; r++) {
            indexes[r] = arr.getLowBound(r);
            size *= arr.getSize(r);
        }
        int hash = 1;
        for (int i = 0; i < size; i++) {
            Object element = arr.get(indexes);
            hash = 31*hash + (element==null ? 0 : element.hashCode());
            gnu.lists.Arrays.incrementIndexes(indexes, arr);
        }
        return hash;
    }

    public static Array<Integer> asIntArrayOrNull(Object obj) {
        if (obj instanceof Array) {
            Array arr = (Array) obj;
            int kind = arr.getElementKind();
            if (kind >= Sequence.INT_U8_VALUE && kind <= Sequence.INT_S64_VALUE)
                return arr;
        }
        if (obj instanceof Array) {
            Array arr = (Array) obj;
            int sz = arr.getSize();
            int rank = arr.rank();
            int[] data = new int[sz];
            int[] dims = new int[rank];
            int[] lows = null;
            int[] work = new int[rank];
            for (int i = 0; i < rank; i++) {
                dims[i] = arr.getSize(i);
                int low = arr.getLowBound(i);
                if (low != 0) {
                    if (lows == null)
                        lows = new int[rank];
                    lows[i] =  low;
                }
                work[i] = low;
            }
            for (int j = 0;  j < sz; ) {
                Object datum = arr.get(work);
                data[j] = ((Number) datum).intValue();
                if (++j == sz)
                    break;
                work[rank-1] ++;
                for (int i = rank; --i >= 0; ) {
                    int low = lows == null ? 0 : lows[i];
                    if (work[i] < low + dims[i])
                        break;
                    work[i] = low;
                    work[i-1]++;
                }
            }
            S32Vector vec = new S32Vector(data);
            if (rank == 1 && lows == null)
                return vec;
            return (Array<Integer>) new GeneralArray(vec, dims, lows);
        }
        IntSequence is = Sequences.asIntSequenceOrNull(obj);
        if (is != null)
            return is;
        if (obj instanceof Number) {
            // FIXME should use an optimized class for this case
            int[] iis = new int[] { ((Number) obj).intValue() };
            S32Vector vec = new S32Vector(iis);
            return (Array<Integer>)
                new GeneralArray(vec, AbstractSequence.noInts,
                                 AbstractSequence.noInts);
        }
        return null;
    }

    public static void incrementIndexes(int[] indexes, Array<?> arr) {
        for (int r = arr.rank(); --r >= 0; ) {
            int ind = indexes[r];
            ind++;
            int low = arr.getLowBound(r);
            int dim = arr.getSize(r);
            if (ind-low < dim) {
                indexes[r] = ind;
                break;
            }
            indexes[r] = low;
        }
    }

    public static int[] getDimensions(Array<?> arr) {
        if (arr instanceof GeneralArray<?>)
            return ((GeneralArray<?>) arr).dimensions;
        int rank = arr.rank();
        int[] dims = new int[rank];
        for (int i = rank; --i >= 0; )
            dims[i] = arr.getSize(i);
        return dims;                             
    }

     public static int[] getLowBounds(Array<?> arr) {
        if (arr instanceof GeneralArray<?>)
            return ((GeneralArray<?>) arr).lowBounds;
        int rank = arr.rank();
        int[] lows = new int[rank];
        for (int i = rank; --i >= 0; )
            lows[i] = arr.getLowBound(i);
        return lows;                             
    }

    public static <E> void fill(Array<E> arr, E value) {
        int rank = arr.rank();
        int[] indexes = new int[rank];
        for (int r = rank; --r >= 0; )
            indexes[r] = arr.getLowBound(r);
        int size = arr.getSize();
        for (int i = size; --i >= 0; ) {
            arr.set(indexes, value);
            incrementIndexes(indexes, arr);
        }
    }

    public static <E> void copy(Array<E> dst,  Array<E> src) {
        int rank = dst.rank();
        if (rank != src.rank())
            throw new RuntimeException("incompatible arrays for copy (source rank :"+src.rank()+", destination rank:"+rank+")");
        int[] idst = new int[rank];
        int[] isrc = new int[rank];
        for (int r = rank; --r >= 0; ) {
            int ssize = src.getSize(r);
            int dsize = dst.getSize(r);
            if (ssize != dsize)
                throw new RuntimeException("incompatible arrays for copy, dimension "+r+" (source size: "+ssize+"; destination: "+dsize+")");
            isrc[r] = src.getLowBound(r);
            idst[r] = dst.getLowBound(r);
        }
        int size = dst.getSize();
        for (int i = size; --i >= 0; ) {
            E value = src.get(isrc);
            dst.set(idst, value);
            incrementIndexes(isrc, src);
            incrementIndexes(idst, dst);
        }
    }

   public static <E> GeneralArray<E> simpleCopy(Array<E> arr, boolean writable) {
        SimpleVector<E> vec = flattenCopy(arr, writable);
        return GeneralArray.make(vec, getDimensions(arr),
                                 getLowBounds(arr), null, 0);
    }

    public static <E> SimpleVector<E> flattenCopy(Array<E> arr, boolean writable) {
        int rank = arr.rank();
        int[] indexes = new int[rank];
        for (int d = rank; --d >= 0; )
            indexes[d] = arr.getLowBound(d);
        int size = arr.getSize();
        int kind = arr.getElementKind();
        SimpleVector vec;
        switch (kind) {
        case Sequence.BOOLEAN_VALUE:
            {
                boolean[] data = new boolean[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getBooleanRaw(effi);
                }
                vec = new BitVector(data);
            }
            break;
         case Sequence.CHAR_VALUE:
            {
                char[] data = new char[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getCharRaw(effi);
                }
                vec = new CharVector(data);
            }
            break;
        case Sequence.INT_S8_VALUE:
        case Sequence.INT_U8_VALUE:
            {
                byte[] data = new byte[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getByteRaw(effi);
                }
                vec = kind == Sequence.INT_S8_VALUE ? new S8Vector(data)
                    : new U8Vector(data);
            }
            break;
        case Sequence.INT_S16_VALUE:
        case Sequence.INT_U16_VALUE:
            {
                short[] data = new short[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getShortRaw(effi);
                }
                vec = kind == Sequence.INT_S16_VALUE ? new S16Vector(data)
                    : new U16Vector(data);
            }
            break;
        case Sequence.INT_S32_VALUE:
        case Sequence.INT_U32_VALUE:
            {
                int[] data = new int[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getIntRaw(effi);
                }
                vec = kind == Sequence.INT_S32_VALUE ? new S32Vector(data)
                    : new U32Vector(data);
            }
            break;
        case Sequence.INT_S64_VALUE:
        case Sequence.INT_U64_VALUE:
            {
                long[] data = new long[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getLongRaw(effi);
                }
                vec = kind == Sequence.INT_S64_VALUE ? new S64Vector(data)
                    : new U64Vector(data);
            }
            break;
       case Sequence.FLOAT_VALUE:
            {
                float[] data = new float[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getFloatRaw(effi);
                }
                vec = new F32Vector(data);
            }
            break;
        case Sequence.DOUBLE_VALUE:
            {
                double[] data = new double[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getDoubleRaw(effi);
                }
                vec = new F64Vector(data);
            }
            break;
        default:
            {
                Object[] data = new Object[size];
                for (int i = 0;  i < size; i++) {
                    int effi = arr.effectiveIndex(indexes);
                    incrementIndexes(indexes, arr);
                    data[i] = arr.getRaw(effi);
                }
                vec = new FVector(data);
            }
            break;
        }
        if (! writable)
            vec.info |= SimpleVector.READ_ONLY_FLAG;
        return vec;
    }
}
