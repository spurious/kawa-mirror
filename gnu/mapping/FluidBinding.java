// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** Used for a stack of fluid bindings. */

public class FluidBinding
{
  /** The Symbol which this overrides. */
  Symbol symbol;

  /** The current value of the Symbol in this thread. */
  public Object value;

  /** The maininin bindings active in this thread. */
  FluidBinding previous;

  public FluidBinding (FluidBinding previous, Object value, Symbol symbol)
  {
    this.previous = previous;
    this.value = value;
    this.symbol = symbol;
  }

  public static FluidBinding make (FluidBinding prev,
				   Object val, Symbol symbol)
  {
    return new FluidBinding(prev, val, symbol);
  }
}
