// Copyright (c) 2002, 2003, 2016  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.bytecode.PrimType;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.math.IntNum;

/** Static methods for implementing Scheme (SRFI-25) arrays. */

public class Arrays
{
  static final int[] shapeStrides = { 2, 1} ;
  static final int[] zeros2 = new int[2];

  /** Convert a sequence of (lower,upper) bounds to a SRFI-25 shape.
    */
  public static Array shape (Object[] vals)
  {
    int len = vals.length;
    if ((len & 1) != 0)
      throw new RuntimeException("shape: not an even number of arguments");
    int d = len >> 1;
    int[] dims = { d, 2 };
    return new GeneralArray(new FVector(vals), dims, null, shapeStrides, 0);
  }

    /** Convenience method for resolving shape specifiers.
     */
    public static GeneralArray allocateArray(Array shape) {
        int srank = shape.rank();
        int rank = shape.getSize(0);
        if (srank != 1
            && ! (srank == 2 && shape.getSize(1) == 2))
            throw new RuntimeException("array shape must be a sequence or a rank*2 array");  
        int[] dimensions = new int[rank];
        int[] lowBounds = null;
        for (int i = rank;  --i >= 0; ) {
            int lo, hi;
            if (srank == 2) {
                lo = shape.getInt(i, 0);
                hi = shape.getInt(i, 1);
            } else if (shape instanceof IntSequence) {
                lo = 0;
                hi = ((IntSequence) shape).getInt(i);
            } else {
                Object dim = shape.get(i);
                Sequence sdim;
                if (dim instanceof Range.IntRange) {
                    Range.IntRange range = (Range.IntRange) dim;
                    if (range.getStepInt() != 1)
                        ; // ERROR
                    lo = range.getStartInt();
                    hi = range.size() + lo;
                } else {
                    lo = 0;
                    hi = ((Number) dim).intValue();
                }
            }
            if (lo > hi)
                throw new RuntimeException("array dimension "+i+" has negative size"); 
            dimensions[i] = hi - lo;
            if (lo != 0) {
                if (lowBounds == null)
                    lowBounds = new int[rank];
                lowBounds[i] = lo;
            }
        }
        return GeneralArray.make(null, dimensions, lowBounds, null, 0);
    }

    public static Array makeFromValues(Array shape, Object[] values) {
        GeneralArray array = allocateArray(shape);
        int total = array.getSize();
        Object[] data = new Object[total];
        if (values != null && values.length > 0) {
            int j = 0;
            for (int i = 0;  i < total; ++i) {
                data[i] = values[j];
                if (++j == values.length)
                    j = 0;
            }
        }
        array.setBase(data);
        return array;
    }

    public static Array makeFromSimple(int [] dimensions, int[] lowBounds,
                                       Object buffer,
                                       PrimType elementType) {
        char sig1;
        if (elementType == null)
            sig1 = 'L';
        else {
            sig1 = elementType.getSignature().charAt(0);
            if (elementType.isUnsigned())
                sig1 = Character.toLowerCase(sig1);
        }
        int rank = dimensions.length;
        SimpleVector base;
        switch (sig1) {
        case 'L':
            base = new FVector((Object[]) buffer);
            break;
        case 'B':
            base = new S8Vector((byte[]) buffer);
            break;
        case 'b':
            base = new U8Vector((byte[]) buffer);
            break;
        case 'I':
            base = new S32Vector((int[]) buffer);
            break;
        case 'i':
            base = new U32Vector((int[]) buffer);
            break;
        case 'J':
            base = new S64Vector((long[]) buffer);
            break;
        case 'j':
            base = new U64Vector((long[]) buffer);
            break;
        case 'S':
            base = new S16Vector((short[]) buffer);
            break;
        case 's':
            base = new U16Vector((short[]) buffer);
            break;
        case 'D':
            base = new F64Vector((double[]) buffer);
            break;
        case 'F':
            base = new F32Vector((float[]) buffer);
            break;
        default:
            throw new Error("bad type for makeFromSimple");
        }
        if (rank == 1 && (lowBounds == null || lowBounds[0] == 0))
            return base;
        else
            return GeneralArray.makeSimple(lowBounds, dimensions, base);
    }

    public static Array makeSimple(Array shape, SimpleVector base) {
        GeneralArray array = allocateArray(shape);
        array.setBase(base);
        return array;
  }

  private static int effectiveIndex(Array array, Procedure proc,
				    Object[] args, int[] work)
    throws Throwable
  {
    Object mapval = proc.applyN(args);
    if (mapval instanceof Values)
      {
	Values mapvals = (Values) mapval;
	for (int i = 0, j = 0;  (i = mapvals.nextPos(i)) != 0;  j++)
	  {
	    work[j] = ((Number) mapvals.getPosPrevious(i)).intValue();
	  }
      }
    else
      work[0] = ((Number) mapval).intValue();
    if (array instanceof GeneralArray)
        return ((GeneralArray) array).resolve(work);
    else
        return work[0];
  }

  /* Create a view of a simple-array that is an affine index transform.
   */
  public static Array shareArray(Array array, Array shape,
				 Procedure proc)
    throws Throwable
  {
    GeneralArray result = allocateArray(shape);
    int rank = result.rank();
    Object[] args = new Object[rank];
    int[] dimensions = result.getDimensions();
    int[] lowBounds = result.getLowBounds();
    boolean empty = result.getSize() == 0;
    for (int i = rank;  --i >= 0; )
        args[i] = Integer.valueOf(result.getLowBound(i));
    int arank = array.rank();
    int[] offsets = new int[rank];
    int offset0;
    if (empty)
      offset0 = 0;
    else
      {
	int[] work = new int[arank];
	offset0 = effectiveIndex (array, proc, args, work);
	for (int i = rank;  --i >= 0; )
	  {
	    int size = dimensions[i];
	    int lo = lowBounds == null ? 0 : lowBounds[i];
	    if (size <= 1)
	      offsets[i] = 0;
	    else
	      {
		Object low = args[i];
		args[i] = IntNum.make(lo + 1);
		offsets[i] = (effectiveIndex (array, proc, args, work)
			      - offset0);
		args[i] = low;
	      }
	  }
      }
    AVector base = array instanceof GeneralArray
        ? ((GeneralArray) array).getBase()
        : (AVector) array;
    result.setBase(base);
    result.setStrides(offsets, offset0);
    return result;
  }

    /** Class for implementing computed (virtual) array.
     * Used by build-array procedure.
     */
    public static class BuiltArray<E> extends AbstractSequence<E>
        implements Array<E> {
        Procedure proc;
        int[] dims;
        int[] lowBounds;
        public BuiltArray(Procedure proc,
                          int[] dimensions, int[] lowBounds) {
            
            this.proc = proc;
            this.dims = dimensions;
            this.lowBounds = lowBounds;
        }
        @Override
        public int rank() { return dims.length; }
        @Override
        public int getLowBound(int dim) { return lowBounds[dim]; }
        @Override
        public int getSize(int dim) { return dims[dim]; }

        public E get() { return get(AbstractSequence.noInts); }
        public E get(int i) { return get(new int[] {i}); }
        public E get(int i, int j) { return get(new int[] {i, j}); }
        public E get(int i, int j, int k, int... rest) {
            if (rest.length == 0)
                return get(new int[]{i, j, k});
            int[] indexes = new int[rest.length+3];
            indexes[0] = i;
            indexes[1] = j;
            indexes[2] = k;
            System.arraycopy(rest, 0, indexes, 0, rest.length-3);
            return get(indexes);
        } 

        public E get(int[] indexes) {
            try {
                return (E) proc.apply1(new S32Vector(indexes));
            } catch (Throwable ex) {
                throw new RuntimeException("caught "+ex+" evaluating array procedure", ex);
            }
        }
        public E getRaw(int effi) {
            int[] indexes = new int[rank()];
            for (int i = rank(); --i >= 0; ) {
                int d = dims[i];
                indexes[i] = (effi % d) + lowBounds[i];
                effi = effi / d;
            }
            return get(indexes);
        }
    }
    public static <E> Array<E> getBuiltArray(Array shape, Procedure procedure) {
        GeneralArray ashape = allocateArray(shape);
        return new BuiltArray(procedure,
                              ashape.getDimensions(), ashape.getLowBounds());
    }

    /** General array "view" class using a Procedure index transformer.
     * Used by array-transform procedure.
     */
    public static class ProcTransformedArray<E> extends TransformedArray<E> {
        Procedure proc;
        int[] dims;
        int[] lowBounds;
        public ProcTransformedArray(Array<E> base, Procedure transformer,
                                    int[] dimensions, int[] lowBounds) {
            super(base);
            this.proc = transformer;
            this.dims = dimensions;
            this.lowBounds = lowBounds;
        }
        @Override
        public int rank() { return dims.length; }
        @Override
        public int getLowBound(int dim) { return lowBounds[dim]; }
        @Override
        public int getSize(int dim) { return dims[dim]; }

        public final int effectiveIndex(int i, int j) {
            return effectiveIndex(new int[] {i, j});
        }
        public int effectiveIndex(int[] indexes) {
            Object obj;
            try {
                obj = proc.apply1(indexes);
            } catch (Throwable ex) {
                throw new RuntimeException("index transformer throw "+ex, ex);
            }
            if (obj instanceof int[])
                return base.effectiveIndex((int[]) obj);
            IntSequence ind = Sequences.asIntSequenceOrNull(obj);
            if (ind == null)
                throw new ClassCastException("not an integer sequence");
            if (ind instanceof S32Vector)
                return base.effectiveIndex(((S32Vector) ind).getBuffer());
            else {
                int rank = ind.size();
                switch (rank) {
                case 0:
                    return base.effectiveIndex();
                case 1:
                    return base.effectiveIndex(ind.getInt(0));
                case 2:
                    return base.effectiveIndex(ind.getInt(0), ind.getInt(1));
                default:
                    int[] rest = rank == 3 ? AbstractSequence.noInts
                        : new int[rank-3];
                    for (int i = 3; i < rank; i++)
                        rest[i-3] = ind.getInt(i);
                    return base.effectiveIndex(ind.getInt(0), ind.getInt(1),
                                               ind.getInt(2), rest);
                }
            }
        }
    }
    public static <E> Array<E> getTransformed(Array<E> base, Procedure transformer, Array shape) {
        GeneralArray ashape = allocateArray(shape);
        return new ProcTransformedArray(base, transformer,
                                        ashape.getDimensions(),
                                        ashape.getLowBounds());
    }
}
