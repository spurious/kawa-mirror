package gnu.mapping;

/** This is a constraint used to catch unbound variables. */

public class UnboundConstraint extends Constraint
{
  Environment environment;

  static UnboundConstraint instance;

  public static UnboundConstraint getInstance(Environment environment)
  {
    UnboundConstraint result;
    if (environment != null)
      {
	result = environment.unboundConstraint;
	if (result == null)
	  {
	    result = new UnboundConstraint(environment);
	    environment.unboundConstraint = result;
	  }
      }
    else
      {
	result = instance;
	if (result == null)
	  {
	    result = new UnboundConstraint(null);
	    instance = result;
	  }
      }
    return result;
  }

  public static UnboundConstraint getInstance(Binding binding)
  {
    Constraint constraint = binding.constraint;
    if (constraint instanceof UnboundConstraint)
      return (UnboundConstraint) binding.constraint;
    return getInstance(constraint.getEnvironment(binding));
  }

  public UnboundConstraint (Environment environment)
  {
    this.environment = environment;
  }

  public Object get (Binding binding, Object defaultValue)
  {
    // Before reporting an error, check parent environment.
    Object value = binding.value;
    if (value == null && environment != null
	&& environment.previous != null)
      binding.value = value = environment.previous.lookup(binding.getName());
    if (value != null)
      return ((Binding) value).get();
    return defaultValue;
  }

  public boolean isBound (Binding binding)
  {
    return false;
  }

  public void set (Binding binding, Object value)
  {
    Environment env = getEnvironment(binding);
    if (env != null && env.locked)
      throw new IllegalStateException("attempt to modify variable: "
				      + binding.getName()
				      + " locked environment");
    synchronized (binding)
      {
	if (binding.constraint == this)
	  binding.setConstraint(TrivialConstraint.getInstance(env));
	binding.constraint.set(binding, value);
      }
  }

  public Environment getEnvironment (Binding binding)
  {
    return environment;
  }
}
