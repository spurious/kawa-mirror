package gnu.mapping;

/** This is the default Constraint for a simple variable Binding. */

public class TrivialConstraint extends Constraint
{
  Environment environment;

  public TrivialConstraint (Environment environment)
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
    binding.value = value;
  }

  public Environment getEnvironment (Binding binding)
  {
    return environment;
  }
}
