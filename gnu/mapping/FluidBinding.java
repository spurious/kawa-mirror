// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** Used for a stack of fluid bindings. */

public class FluidBinding
{
  /** The Binding which this overrides. */
  Binding binding;

  /** The current value of the Binding in this thread. */
  public Object value;

  /** The maininin bindings active in this thread. */
  FluidBinding previous;

  public FluidBinding (FluidBinding previous, Object value, Binding binding)
  {
    this.previous = previous;
    this.value = value;
    this.binding = binding;
  }

  public static FluidBinding make (FluidBinding prev, Object val, Binding bind)
  {
    return new FluidBinding(prev, val, bind);
  }
}
