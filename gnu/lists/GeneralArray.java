// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** An UNFINISHED class to handle general multi-dimensional arrays.
 * If the number of dimensions (the "rank") is one, should use
 * a class that implements Sequence.
 * GeneralArray uses a SimpleVector 'base' to store the actual data, and
 * provides general linear mapping from the array indexes to an
 * element index in the 'base' SimpleVector.  Thus such uperations as
 * transposing an array can be implement as just creating a simple
 * re-mapping of the indexes. */

public class GeneralArray
// Should extends AbstractSequence?
implements Array //, Consumable
// Should implement Collection?
// Note this intentionally does not implement Sequence.
{
  SimpleVector base;
  int rank;
  int[] dimensions;
  int[] stride;
  int offset;

  public GeneralArray(int[] dimensions)
  {
    int total = 1;
    int rank = dimensions.length;
    int[] stride = new int[rank];
    for (int i = rank;  --i >= 0; )
      {
	stride[i] = total;
	total *= dimensions[i];
      }
    base = new FVector(total);
    this.dimensions = dimensions;
    this.rank = rank;
    this.offset = 0;
  }

  public int rank() { return rank; }

  public int getEffectiveIndex(int[] indexes)
  {
    int result = offset;
    for (int i = rank;  --i >= 0; )
      {
	int index = indexes[i];
	if (index < 0 || index >= dimensions[i])
	  throw new IndexOutOfBoundsException();
	result += stride[i] * index;
      }
    return result;
  }

  public Object get(int[] indexes)
  {
    return base.get(getEffectiveIndex(indexes));
  }

  public Object set(int[] indexes, Object value)
  {
    return base.set(getEffectiveIndex(indexes), value);
  }

  /** See java.util.Collection. */
  public int size()
  {
    int total = 1;
    for (int i = rank;  --i >= 0; )
      total *= dimensions[i];
    return total;
  }
}
