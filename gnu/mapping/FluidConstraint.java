// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** This Constraint searches a thread's stack of FluidBindings. */

public class FluidConstraint extends Constraint
{
  /** The original Constraint from the Symbol.
   * Use this to get the default value if there is no fluid binding
   * in the current thread. */
  Constraint savedConstraint;

  /** Number of active FluidBinding objects for our Symbol. */
  int referenceCount;

  public FluidConstraint (Constraint savedConstraint)
  {
    this.savedConstraint = savedConstraint;
  }

  FluidBinding find (Symbol symbol)
  {
    FluidBinding fl = CallContext.getInstance().fluidBindings;
    for (; fl != null;  fl = fl.previous)
      {
	if (fl.symbol == symbol)
	  return fl;
      }
    return null;
  }

  public Object get (Symbol binding, Object defaultValue)
  {
    FluidBinding fl = find(binding);
    if (fl == null)
      return savedConstraint.get(binding, defaultValue);
    else
      return fl.value;
  }

  public void set (Symbol binding, Object value)
  {
    FluidBinding fl = find(binding);
    if (fl == null)
      savedConstraint.set(binding, value);
    else
      fl.value = value;
  }

}
