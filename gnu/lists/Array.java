// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** General interface to arrays of arbitrary dimension. */

public interface Array
{
  /**
   * Get the rank (number of dimensions) of this array.
   * The rank of a scalar is 0, of a Sequence is 1, of a matrix is 2, etc.
   */
  public int rank();

  public int getEffectiveIndex(int[] indexes);

  public Object get(int[] indexes);

  public Object set(int[] indexes, Object value);
}
