// Copyright (c) 1998  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location is an abstract cell/location/variable with a value. */

public abstract class Location extends Procedure0 implements HasSetter
{
  public abstract Object get ();

  public abstract void set (Object value);

  public Object apply0 () { return get(); }
  public void set0 (Object value) { set(value); }
}
