package gnu.lists;

/** View an array as a vector, with the former's elements in row-major order. */

public class FlattenedArray<E> extends TransformedArray<E> implements AVector<E>
{
    private final int size;
    private final int brank;

    public FlattenedArray(Array<E> base) {
        super(base);
        size = base.getSize();
        brank = base.rank();
    }

    @Override
    public int size() { return size; }

    @Override
    public int getSize(int dim) {
        if (dim != 0)
            badRank(dim);
        return size;
    }

    @Override
    public int effectiveIndex(int i) {
        return Arrays.rowMajorToEffectiveIndex(base, i);
    }

    /** Created a shared flattened view of the argument.
     */
    public static <E> AVector<E> flatten(Array<E> array) {
        if (array instanceof AVector)
            return (AVector) array;
        if (array instanceof GeneralArray) {
            GeneralArray<E> garr = (GeneralArray<E>) array;
            if (garr.simple && garr.base instanceof AVector)
                return (AVector<E>) garr.base;
        }
        return new FlattenedArray(array);
    }
}
