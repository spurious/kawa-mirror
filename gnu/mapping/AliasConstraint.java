// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** The value field of a AliasConstraint points to another Binding. */

public class AliasConstraint extends Constraint
{
  public Object get (Binding binding, Object defaultValue)
  {
    if (binding.value instanceof Binding)
      return ((Binding) binding.value).get(defaultValue);
    try
      {
	return ((Location) binding.value).get();
      }
    catch (UnboundSymbol ex)
      {
	return defaultValue;
      }
  }

  public void set (Binding binding, Object value)
  {
    ((Location) binding.value).set(value);
  }

  public boolean isBound (Binding binding)
  {
    return ((Location) binding.value).isBound();
  }

  public Object getFunctionValue(Binding binding)
  {
    return ((Binding) binding.value).getFunctionValue();
  }

  public static Binding followAliases(Binding binding)
  {
    while (binding.constraint instanceof AliasConstraint)
      binding = (Binding) binding.value;
    return binding;
  }

  public static void define (Binding binding, Location location)
  {
    synchronized (binding)
      {
	binding.value = location;
	binding.constraint = new AliasConstraint();  // FIXME share?
      }
  }
}
