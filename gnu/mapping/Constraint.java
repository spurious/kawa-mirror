package gnu.mapping;

/** A Constraint is used to control the values in a Binding.
 * This is a very general mechanism, since you can change the Constaint
 * associated with a Binding as needed. */

public abstract class Constraint
{
  public abstract Object get (Binding binding);

  public abstract void set (Binding binding, Object value);

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
}
