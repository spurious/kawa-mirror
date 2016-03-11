// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** General interface to arrays of arbitrary dimension. */

public interface Array<E>
{
  public boolean isEmpty();

  /**
   * Get the rank (number of dimensions) of this array.
   * The rank of a scalar is 0, of a Sequence is 1, of a matrix is 2, etc.
   */
  public int rank();
    public int getElementKind();
    int effectiveIndex();
    int effectiveIndex(int index);
    int effectiveIndex(int i, int j);
    int effectiveIndex(int i, int j, int k, int... rest);
    public int effectiveIndex(int[] indexes);

    /** Given an "effective index", return element as object. */
    E getRaw(int index);
    boolean getBooleanRaw(int index);
    char getCharRaw(int index);
    byte getByteRaw(int index);
    short getShortRaw(int index);
    int getIntRaw(int index);
    long getLongRaw(int index);
    float getFloatRaw(int index);
    double getDoubleRaw(int index);
    void setRaw(int index, E value);
    public E get();
    public E get(int i);
    public E get(int i, int j);
    public E get(int i, int j, int k, int... rest);
    public E get(int[] indexes);

    public void set(int[] indexes, E value);

    public int getInt();
    public int getInt(int arg1);
    public int getInt(int arg1, int arg2);
    public int getInt(int arg1, int arg2, int arg3, int... rest);
    public int getInt(int[] args);

    public E getRowMajor(int index);

    public Array<E> asImmutable();

    /** Get the least dimension along the specified dimension. */
    public int getLowBound(int dim);

    /** Get length along specified dimension. */
    public int getSize(int dim);

    /** Total number of elements.
     * Same as the product of getSize(S) for all S. */
    public int getSize();
}
