// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/**
 * A Binding that contains a separate Function Value component.
 * Useful to implement Symbols in languages like Common Lisp and
 * Emacs Lisp where there are are separate namespace for values and functions.
 */

public class Binding2 extends Binding
{
  public Object functionValue;

  public Binding2 (String name)
  {
    super(name);
  }

  public static Binding2 getBinding2 (Environment env, String name)
  {
    Binding binding = env.lookup(name);
    Binding2 binding2;
    if (binding == null)
      {
	binding2 = new Binding2(name);
	binding2.constraint = env.unboundConstraint;
	env.addBinding(binding2);
	return binding2;
      }
    while (binding.constraint instanceof AliasConstraint)
      binding = (Binding) binding.value;
    if (binding instanceof Binding2)
      return (Binding2) binding;
    binding2 = new Binding2(name);
    binding2.constraint = binding.constraint;
    binding2.value = binding.value;
    System.err.println("getB2 "+name+" b2:"+binding2);
    binding.value = binding2;
    binding.constraint = new AliasConstraint();  // FIXME;
    return binding2;
  }
}
