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
    binding.value = binding2;
    binding.constraint = new AliasConstraint();  // FIXME - should share;
    env.addBinding(binding2);
    return binding2;
  }

  public Procedure getProcedure()
  {
    if (functionValue == null)
      {
	String n = getName();
	throw new UnboundSymbol(n,
				"Symbol's function definition is void: " + n);
      }
    try
      {
	return (Procedure) functionValue;
      }
    catch (ClassCastException ex)
      {
	throw new WrongType(getName(), WrongType.ARG_DESCRIPTION, ex);
      }
  }

  /* Don't know if these make sense.  If so, may ApplyExp call them.  FIXME.
  public Object apply0 ()
  {
    return (getProcedure()).apply0();
  }

  public Object apply1 (Object arg1)
  {
    return (getProcedure()).apply1(arg1);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return (getProcedure()).apply2(arg1, arg2);
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    return (getProcedure()).apply3(arg1, arg2, arg3);
  }

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
  {
    return (getProcedure()).apply4(arg1, arg2, arg3, arg4);
  }

  public Object applyN (Object[] args)
  {
    return (getProcedure()).applyN(args);
  }
  */
}
