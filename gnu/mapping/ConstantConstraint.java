// Copyright (c) 2001 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A constraint for an immutable binding. */

public class ConstantConstraint extends Constraint
{
  Environment environment;

  static ConstantConstraint instance;

  public static ConstantConstraint getInstance(Environment environment)
  {
    ConstantConstraint result;
    if (environment != null)
      {
	result = environment.constantConstraint;
	if (result == null)
	  {
	    result = new ConstantConstraint(environment);
	    environment.constantConstraint = result;
	  }
      }
    else
      {
	result = instance;
	if (result == null)
	  {
	    result = new ConstantConstraint(null);
	    instance = result;
	  }
      }
    return result;
  }

  public static ConstantConstraint getInstance(Binding binding)
  {
    Constraint constraint = binding.constraint;
    if (constraint instanceof ConstantConstraint)
      return (ConstantConstraint) binding.constraint;
    return getInstance(constraint.getEnvironment(binding));
  }

  public ConstantConstraint (Environment environment)
  {
    this.environment = environment;
  }

  public boolean isBound (Binding binding)
  {
    return true;
  }

  public Object get (Binding binding)
  {
    return binding.value;
  }

  public void set (Binding binding, Object value)
  {
    if (binding.value == value)
      return; // No change - ignore
    throw new IllegalStateException("attempt to modify read-only variable: "
				    + binding.getName());
  }

  public Environment getEnvironment (Binding binding)
  {
    return environment;
  }
}
