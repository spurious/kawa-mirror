// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location suitable when Environment or Location can be access by
 * multiple threads.  Accesses are synchronized.
 * Also, setWithSave setRestore are different than PlainLocation. */

public class SharedLocation extends IndirectableLocation
{
  int timestamp;

  public SharedLocation (Symbol symbol, Object property, int timestamp)
  {
    super(symbol, property);
    this.timestamp = timestamp;
  }

  public synchronized final Object get (Object defaultValue)
  {
    return base != null ? base.get(defaultValue)
      : value == Location.UNBOUND ? defaultValue : value;
  }

  public synchronized boolean isBound ()
  {
    return base != null ? base.isBound() : value != Location.UNBOUND;
  }

  public synchronized final void set (Object newValue)
  {
    if (base == null)
      value = newValue;
    else if (value == DIRECT_ON_SET)
      {
	base = null;
	value = newValue;
      }
    else if (base.isConstant())
      getEnvironment().put(getKeySymbol(), getKeyProperty(), newValue);
    else
      base.set(newValue);
  }

  public synchronized Object setWithSave (Object newValue)
  {
    Object old;
    if (base != null)
      {
	old = base;
	base = null;
      }
    else
      {
	old = value;
      }
    value = newValue;
    return old;
  }

  public synchronized void setRestore (Object oldValue)
  {
    if (base != null)
      base.setRestore(oldValue);
    else if (oldValue instanceof Location)
      {
	value = null;
	base = (Location) oldValue;
      }
    else
      value = oldValue;
  }

}
