// Copyright (c) 2000, 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class LocationEnumeration
  implements
  /* BEGIN JAVA2 */
  java.util.Iterator,
  /* END JAVA2 */
  java.util.Enumeration
{
  Environment env;
  NamedLocation curLoc;
  /** Index field used by Environment.hasMoreElements.
      If inherited==null, index in bindings, else index in env.inherited. */
  int index;
  LocationEnumeration inherited;
  NamedLocation[] bindings;

  public LocationEnumeration(NamedLocation[] bindings, int count)
  {
    this.bindings = bindings;
    index = count;
  }

  public LocationEnumeration(SimpleEnvironment env)
  {
    this(env.table, 1 << env.log2Size);
  }

  public boolean hasMoreElements()
  {
    return env.hasMoreElements(this);
  }

  public Object nextElement()
  {
    return nextLocation();
  }

  public Location nextLocation()
  {
    if (curLoc == null && ! hasMoreElements())
      throw new java.util.NoSuchElementException();
    Location r = curLoc;
    if (r instanceof UnboundLocation)
      r = ((UnboundLocation) r).getLocation();
    curLoc = curLoc.next;
    return r;
  }

  public boolean hasNext ()
  {
    return hasMoreElements();
  }

  public Object next ()
  {
    return nextElement();
  }

  public void remove ()
  {
    throw new Error();  // FIXME
    //env.remove(loc);
  }
}
