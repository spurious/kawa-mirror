// Copyright (c) 2001 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A constraint for an immutable symbol. */

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

  public static ConstantConstraint getInstance(Symbol symbol)
  {
    Constraint constraint = symbol.constraint;
    if (constraint instanceof ConstantConstraint)
      return (ConstantConstraint) symbol.constraint;
    return getInstance(constraint.getEnvironment(symbol));
  }

  public ConstantConstraint (Environment environment)
  {
    this.environment = environment;
  }

  public boolean isBound (Symbol symbol)
  {
    return true;
  }

  public Object get (Symbol symbol, Object defaultValue)
  {
    return symbol.value;
  }

  public void set (Symbol symbol, Object value)
  {
    if (symbol.value == value)
      return; // No change - ignore
    throw new IllegalStateException("attempt to modify read-only variable: "
				    + symbol.getName());
  }

  public Environment getEnvironment (Symbol symbol)
  {
    return environment;
  }
}
