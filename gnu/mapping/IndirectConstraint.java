package gnu.mapping;

public class IndirectConstraint extends Constraint
{
  public Object get (Binding binding)
  {
    return ((Procedure) binding.value).apply0();
  }

  public void set (Binding binding, Object value)
  {
    ((Procedure) (HasSetter) binding.value).set0(value);
  }

  public static void define (String name, Procedure location)
  {
    define(name, location, Environment.getCurrent());
  }

  public static void define (String name, Procedure location, Environment env)
  {
    Binding binding = env.getBinding(name);
    synchronized (binding)
      {
	binding.value = location;
	binding.constraint = new IndirectConstraint();
      }
  }

  public static void define (Binding binding, Procedure location)
  {
    synchronized (binding)
      {
	binding.value = location;
	binding.constraint = new IndirectConstraint();
      }
  }
}
