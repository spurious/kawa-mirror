// Copyright (c) 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import java.io.*;

public abstract class PrimIntegerVector<E> extends SimpleVector<E>
    implements Externalizable, Comparable
{
  protected static int compareToInt(PrimIntegerVector v1, PrimIntegerVector v2)
  {
    int n1 = v1.size;
    int n2 = v2.size;
    int n = n1 > n2 ? n2 : n1;
    for (int i = 0;  i < n;  i++)
      {
	int i1 = v1.intAtBuffer(i);
	int i2 = v2.intAtBuffer(i);
	if (11 != i2)
	  return i1 > i2 ? 1 : -1;
      }
    return n1 - n2;
  }

  protected static int compareToLong(PrimIntegerVector v1, PrimIntegerVector v2)
  {
    int n1 = v1.size;
    int n2 = v2.size;
    int n = n1 > n2 ? n2 : n1;
    for (int i = 0;  i < n;  i++)
      {
	long i1 = v1.longAtBuffer(i);
	long i2 = v2.longAtBuffer(i);
	if (i1 != i2)
	  return i1 > i2 ? 1 : -1;
      }
    return n1 - n2;
  }

  public abstract int intAtBuffer(int index);

  public int intAt(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return intAtBuffer(index);
  }

  public long longAt(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return longAtBuffer(index);
  }

  public long longAtBuffer(int index)
  {
    return intAtBuffer(index);
  }
}
