// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.math.IntNum;

/** Static methods for implementing Scheme (SRFI-25) arrays. */

public class Arrays
{
  static final int[] shapeStrides = { 2, 1} ;
  static final int[] zeros2 = new int[2];

  public static Array shape (Object[] vals)
  {
    int len = vals.length;
    if ((len & 1) != 0)
      throw new RuntimeException("shape: not an even number of arguments");
    int d = len >> 1;
    int[] dims = { d, 2 };
    return new FVector(vals).transpose(zeros2, dims, 0, shapeStrides);
  }

  public static Array make(Array shape, Object value)
  {
    int rank = shape.getSize(0);
    int[] dimensions = new int[rank];
    int[] lowBounds = null;
    int total = 1;
    for (int i = rank;  --i >= 0; )
      {
	int lo = ((Number) shape.getRowMajor(2*i)).intValue();
	int hi = ((Number) shape.getRowMajor(2*i+1)).intValue();
	int size = hi - lo;
	dimensions[i] = size;
	if (lo != 0)
	  {
	    if (lowBounds == null)
	      lowBounds = new int[rank];
	    lowBounds[i] = lo;
	  }
	total *= size;
      }
    return GeneralArray.makeSimple(lowBounds, dimensions, new FVector(total, value));
  }

  public static Array makeSimple(Array shape, SimpleVector base)
  {
    int rank = shape.getSize(0);
    int[] dimensions = new int[rank];
    int[] lowBounds = null;
    for (int i = rank;  --i >= 0; )
      {
	int lo = ((Number) shape.getRowMajor(2*i)).intValue();
	int hi = ((Number) shape.getRowMajor(2*i+1)).intValue();
	dimensions[i] = hi - lo;
	if (lo != 0)
	  {
	    if (lowBounds == null)
	      lowBounds = new int[rank];
	    lowBounds[i] = lo;
	  }
      }
    return GeneralArray.makeSimple(lowBounds, dimensions, base);
  }

  public static int effectiveIndex (Array array, Procedure proc,
				    Object[] args, int[] work)
    throws Throwable
  {
    Object mapval = proc.applyN(args);
    if (mapval instanceof Values)
      {
	Values mapvals = (Values) mapval;
	int nvals = mapvals.size();
	SeqPosition ivals = mapvals.getIterator();
	for (int j = 0;  j < nvals;  j++)
	  work[j] = ((Number) ivals.next()).intValue();
      }
    else
      work[0] = ((Number) proc.applyN(args)).intValue();
    return array.getEffectiveIndex(work);
  }

  public static Array shareArray(Array array, Array shape,
				 Procedure proc)
    throws Throwable
  {
    int rank = shape.getSize(0);
    Object[] args = new Object[rank];
    int[] dimensions = new int[rank];
    int[] lowBounds = new int[rank]; // null - FIXME
    for (int i = rank;  --i >= 0; )
      {
	Object low = shape.getRowMajor(2*i);
	args[i] = low;
	lowBounds[i] = ((Number) low).intValue();
      }
    int arank = array.rank();
    int[] work = new int[arank];
    int offset0 = effectiveIndex (array, proc, args, work);
    int[] offsets = new int[rank];
    for (int i = rank;  --i >= 0; )
      {
	int hi = ((Number) shape.getRowMajor(2*i+1)).intValue();
	int lo = lowBounds[i];
	int size = hi - lo;
	dimensions[i] = size;
	if (size == 0)
	  offsets[i] = 0;
	else
	  {
	    Object low = args[i];
	    args[i] = IntNum.make(lo + 1);
	    offsets[i] = effectiveIndex (array, proc, args, work) - offset0;
	    args[i] = low;
	  }
      }
    return array.transpose(lowBounds, dimensions, offset0, offsets);
  }
}
