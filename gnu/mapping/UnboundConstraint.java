package gnu.mapping;

/** This is a constraint used to catch unboud variables. */

public class UnboundConstraint extends Constraint
{
  Environment environment;

  public UnboundConstraint (Environment environment)
  {
    this.environment = environment;
  }

  public Object get (Binding binding)
  {
    throw new UnboundSymbol(binding.getName());
  }

  public boolean isBound (Binding binding)
  {
    return false;
  }

  public void set (Binding binding, Object value)
  {
    synchronized (binding)
      {
	if (binding.constraint == this)
	  binding.setConstraint(environment.trivialConstraint);
	binding.constraint.set(binding, value);
      }
  }

  public Environment getEnvironment (Binding binding)
  {
    return environment;
  }
}
