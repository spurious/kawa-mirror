// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class PlainLocation extends NamedLocation
{
  public PlainLocation (Symbol symbol, Object property)
  {
    super(symbol, property);
  }

  public PlainLocation (Symbol symbol, Object property, Object value)
  {
    super(symbol, property);
    this.value = value;
  }

  public final Object get (Object defaultValue)
  {
    return base != null ? base.get(defaultValue)
      : value == Location.UNBOUND ? defaultValue : value;
  }

  public boolean isBound ()
  {
    return base != null ? base.isBound() : value != Location.UNBOUND;
  }

  public final void set (Object newValue)
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
}
