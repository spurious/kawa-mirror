package gnu.mapping;

/** This is the default Constraint for a simple variable Binding. */

public class TrivialConstraint extends Constraint
{
  Environment environment;

  static TrivialConstraint instance;

  public static TrivialConstraint getInstance(Environment environment)
  {
    TrivialConstraint result;
    if (environment != null)
      {
	result = environment.trivialConstraint;
	if (result == null)
	  {
	    result = new TrivialConstraint(environment);
	    environment.trivialConstraint = result;
	  }
      }
    else
      {
	result = instance;
	if (result == null)
	  {
	    result = new TrivialConstraint(null);
	    instance = result;
	  }
      }
    return result;
  }

  public static TrivialConstraint getInstance(Binding binding)
  {
    Constraint constraint = binding.constraint;
    if (constraint instanceof TrivialConstraint)
      return (TrivialConstraint) binding.constraint;
    return getInstance(constraint.getEnvironment(binding));
  }

  public TrivialConstraint (Environment environment)
  {
    this.environment = environment;
  }

  public boolean isBound (Binding binding)
  {
    return true;
  }

  public Object get (Binding binding, Object defaultValue)
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
