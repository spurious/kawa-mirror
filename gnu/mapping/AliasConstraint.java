// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** The value field of a AliasConstraint points to another Location. */

public class AliasConstraint extends Constraint
{
  public Object get (Symbol binding, Object defaultValue)
  {
    return ((Location) binding.value).get(defaultValue);
  }

  public void set (Symbol binding, Object value)
  {
    ((Location) binding.value).set(value);
  }

  public boolean isBound (Symbol binding)
  {
    return ((Location) binding.value).isBound();
  }

  public Object getFunctionValue(Symbol binding)
  {
    return ((Symbol) binding.value).getFunctionValue();
  }

  public static Symbol followAliases(Symbol binding)
  {
    while (binding.constraint instanceof AliasConstraint)
      binding = (Symbol) binding.value;
    return binding;
  }

  public static void define (Symbol binding, Location location)
  {
    synchronized (binding)
      {
	binding.value = location;
	binding.constraint = new AliasConstraint();  // FIXME share?
      }
  }
}
