// Copyright (c) 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;

/** A hash table where "equals" is object identity. */

public class IdentityHashTable extends GeneralHashTable
{
  public IdentityHashTable ()
  {
  }

  public IdentityHashTable (int capacity)
  {
    super(capacity);
  }

  public int hash (Object key)
  {
    return System.identityHashCode(key);
  }

  public boolean matches (Object value1, Object value2)
  {
    return value1 == value2;
  }
}
