// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.lists.*;

/** A Location that can be used as an entry in an Environment. */

public abstract class NamedLocation extends IndirectableLocation
  implements
  /* BEGIN JAVA2 */
  java.util.Map.Entry /* <EnvironmentKey, Object> */,
  /* END JAVA2 */
  EnvironmentKey
{
  NamedLocation next;

  public boolean entered ()
  {
    return next != null;
  }

  public Environment getEnvironment ()
  {
    for (NamedLocation loc = this;  loc != null;  loc = loc.next)
      {
	if (loc.name == null)
	  {
	    Environment env = (Environment) loc.value;
	    if (env != null)
	      return env;
	  }
      }
    return super.getEnvironment();
  }

  final Symbol name;
  final Object property;

  public String toString() { return getClass().getName()+"[#:"+id
      +" name:"+name+(property==null?"":(" prop:"+property))+"]"; }

  public NamedLocation (NamedLocation loc)
  {
    name = loc.name;
    property = loc.property;
  }

  public NamedLocation (Symbol name, Object property)
  {
    this.name = name;
    this.property = property;
  }

  public final Symbol getKeySymbol ()
  {
    return name;
  }

  public final Object getKeyProperty ()
  {
    return property;
  }

  public final boolean matches (EnvironmentKey key)
  {
    return key.getKeySymbol() == this.name
      && key.getKeyProperty() == this.property;
  }

  public final boolean matches (Symbol symbol, Object property)
  {
    return symbol == this.name && property == this.property;
  }

  public final Object /*<EnvironmentKey>*/ getKey ()
  {
    if (property == null)
      return name;
    else
      return this;
  }

  public boolean equals (Object x)
  {
    if (! (x instanceof NamedLocation))
      return false;
    NamedLocation e2 = (NamedLocation) x;
    if (name == null ? e2.name != null : ! name.equals(e2.name))
      return false;
    if (property != e2.property)
      return false;
    Object val1 = getValue();
    Object val2 = e2.getValue();
    if (val1 == val2)
      return true;
    if (val1 == null || val2 == null)
      return false;
    return val1.equals(val2);
  }

  public int hashCode ()
  {
    int h = name.hashCode() ^ System.identityHashCode(property);
    Object val = getValue();
    if (val != null)
      h ^= val.hashCode();
    return h;
  }
}
