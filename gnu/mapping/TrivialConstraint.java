package gnu.mapping;

/** This is the default Constraint for a simple variable Binding. */

public class TrivialConstraint extends Constraint
{
  Environment environment;

  public TrivialConstraint (Environment environment)
  {
    this.environment = environment;
  }

  public Object get (Binding binding)
  {
    return binding.value;
  }

  public void set (Binding binding, Object value)
  {
    binding.value = value;
  }
}
