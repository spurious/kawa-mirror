// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** This Constraint searches a thread's stack of FluidBindings. */

public class FluidConstraint extends Constraint
{
  /** The original Constraint from the Binding.
   * Use this to get the default value if there is no fluid binding
   * in the current thread. */
  Constraint savedConstraint;

  /** Number of active FluidBinding objects for our Binding. */
  int referenceCount;

  public FluidConstraint (Constraint savedConstraint)
  {
    this.savedConstraint = savedConstraint;
  }

  FluidBinding find (Binding binding)
  {
    FluidBinding fl = Future.getFluids();
    for (; fl != null;  fl = fl.previous)
      {
	if (fl.binding == binding)
	  return fl;
      }
    return null;
  }

  public Object get (Binding binding, Object defaultValue)
  {
    FluidBinding fl = find(binding);
    if (fl == null)
      return savedConstraint.get(binding, defaultValue);
    else
      return fl.value;
  }

  public void set (Binding binding, Object value)
  {
    FluidBinding fl = find(binding);
    if (fl == null)
      savedConstraint.set(binding, value);
    else
      fl.value = value;
  }

}
