// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A <code>Location</code> whose value is fixed.
 */

public class ConstantLocation extends NamedLocation
{
  Object value;

  public ConstantLocation (Symbol name, Object property, Object value)
  {
    super(name, property);
    this.value = value;
  }

  public boolean isConstant ()
  {
    return true;
  }

  public final Object get (Object defaultValue)
  {
    return value;
  }

  public final void set (Object newValue)
  {
    throw new IllegalStateException("attempt to modify read-only location: "
				    + getKeySymbol());
  }

}
