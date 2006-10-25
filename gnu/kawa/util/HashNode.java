// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;

/** An entry in a {@link GeneralHashTable}.
 * This is a public class to allow overriding.
 */

public class HashNode
/* #ifdef JAVA2 */
implements java.util.Map.Entry
/* #endif */
{
  public HashNode next;
  int hash;
  Object key;
  Object value;

  public Object get (Object defaultValue)
  {
    return value;
  }

  public Object getKey ()
  {
    return key;
  }

  public Object getValue ()
  {
    return value;
  }

  public Object setValue (Object value)
  {
    Object old = this.value;
    this.value = value;
    return old;
  }

  /** Implements the general Map.Entry specification.
   * But note that a GeneralHashTable subclass may override {@code matches},
   * so it no longer uses equals, in which case it won't be consistent
   * with this method, unless it is overridden. */
  public boolean equals (Object o)
  {
    if (! (o instanceof HashNode))
      return false;
    HashNode h2 = (HashNode) o;
    return (key == null ? h2.key == null : key.equals(h2.key))
      && (value == null ? h2.value == null : value.equals(h2.value));
  }

  /** Implements the general Map.Entry specification.
   * But note that a GeneralHashTable subclass may override {@code hash},
   * so it no longer uses equals, in which case it won't be consistent
   * with this method, unless it is overridden. */
  public int hashCode ()
  {
    return (key ==null ? 0 : key.hashCode())
      ^ (value==null ? 0: value.hashCode());
  }
}
