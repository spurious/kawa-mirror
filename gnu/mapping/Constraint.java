package gnu.mapping;

/** A Constraint is used to control the values in a Binding.
 * This is a very general mechanism, since you can change the Constaint
 * associated with a Binding as needed. */

public abstract class Constraint
{
  public abstract Object get (Binding binding);

  public abstract void set (Binding binding, Object value);

  public boolean isBound (Binding binding)
  {
    try
      {
        get(binding);
        return true;
      }
    catch (UnboundSymbol ex)
      {
        return false;
      }
  }

  public Procedure getProcedure (Binding binding)
  {
    return (Procedure) get(binding);
  }

  /** Return Environment containing a given Binding.
   * Returns null if unknown. */
  public Environment getEnvironment (Binding binding)
  {
    return null;
  }

  protected final static Object getValue(Binding binding)
  {
    return binding.value;
  }

  protected final static void setValue(Binding binding, Object value)
  {
    binding.value = value;
  }

  protected final static Constraint getConstraint(Binding binding)
  {
    return binding.constraint;
  }

  protected final static void setConstraint(Binding binding,
                                            Constraint constraint)
  {
    binding.constraint = constraint;
  }

  /** Get value of "function binding" of a Binding.
   * Some languages (including Common Lisp and Emacs Lisp) associate both
   * a value binding and a function binding with a symbol.
   * @return the function value, or Binding.UNBOUND if no function binding.
   */
  public Object getFunctionValue(Binding binding)
  {
    if (binding instanceof Binding2)
      return ((Binding2) binding).functionValue;
    else if (binding.isBound())
      return binding.get();
    else
      return Binding.UNBOUND;
  }
}
