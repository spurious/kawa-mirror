package gnu.lists;

/** Indexes are mapped. */

public abstract class TransformedArray<E>
        extends AbstractSequence<E> implements Array<E> {
    // Most commonly base is a SimpleVector<E> but not always.
    protected Array<E> base;

    public TransformedArray() { }
    public TransformedArray(Array<E> base) { this.base = base; }

    public int getElementKind() { return base.getElementKind(); }
    public E getRowMajor(int index) { return Arrays.getRowMajor(this, index); }
    
    public E getRaw(int effi) { return base.getRaw(effi); }
    public boolean getBooleanRaw(int effi) { return base.getBooleanRaw(effi); }
    public byte getByteRaw(int effi) { return base.getByteRaw(effi); }
    public char getCharRaw(int effi) { return base.getCharRaw(effi); }
    public short getShortRaw(int effi) { return base.getShortRaw(effi); }
    public int getIntRaw(int effi) { return base.getIntRaw(effi); }
    public long getLongRaw(int effi) { return base.getLongRaw(effi); }
    public float getFloatRaw(int effi) { return base.getFloatRaw(effi); }
    public double getDoubleRaw(int effi) { return base.getDoubleRaw(effi); }
    public int getInt(int i) { return base.getIntRaw(effectiveIndex(i)); }
    public int getInt(int i, int j) { return base.getIntRaw(effectiveIndex(i, j)); }
    public void setRaw(int index, E value) { base.setRaw(index, value); }
    protected void checkCanWrite() {
        if (base instanceof AbstractSequence)
            ((AbstractSequence) base).checkCanWrite();
    }
}
