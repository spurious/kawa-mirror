package gnu.mapping;

public class IndirectConstraint extends Constraint
{
  public Object get (Binding binding, Object defaultValue)
  {
    try
      {
	return ((Procedure) binding.value).apply0();
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  public void set (Binding binding, Object value)
  {
    try
      {
	((Procedure) (HasSetter) binding.value).set0(value);
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
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
