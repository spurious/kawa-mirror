// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** The value field of a AliasConstraint points to another Binding. */

public class AliasConstraint extends Constraint
{
  public Object get (Binding binding)
  {
    binding = (Binding) binding.value;
    return binding.constraint.get(binding);
  }

  public void set (Binding binding, Object value)
  {
    binding = (Binding) binding.value;
    binding.constraint.set(binding, value);
  }
}
