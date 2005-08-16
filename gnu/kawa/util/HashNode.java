// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;
import gnu.mapping.*;

/** An entry in a {@link GeneralHashTable}.
 * This is a public class to allow overriding.
 */

public class HashNode
// extends Location
{
  HashNode next;
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

  public void setValue (Object value)
  {
    this.value = value;
  }
}
