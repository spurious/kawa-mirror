package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.expr.Compilation;

/** A Constraint whose value is that of a named field/method of an object.
 * The object used is the owning Binding's value.
 * (For now, only fields are supported.)
 */

public class ClassMemberConstraint extends Constraint
{
  ClassType type;
  String name;
  java.lang.reflect.Field rfield;

  public ClassMemberConstraint(ClassType type, String name)
  {
    this.type = type;
    this.name = name;
  }

  public ClassMemberConstraint(Class clas, String name)
  {
    this.type = (ClassType) Type.make(clas);
    this.name = name;
  }

  public String getName()
  {
    return name;
  }

  public ClassType getDeclaringClass()
  {
    return type;
  }

  public ClassMemberConstraint(java.lang.reflect.Field field)
  {
    this.rfield = field;
    this.name = field.getName();
  }

  void setup(Binding binding)
  {
    if (rfield == null)
      {
	Class clas;
	try
	  {
	    clas = type.getReflectClass();
	  }
	catch (RuntimeException ex)
	  {
	    String name = binding.getName();
	    throw new UnboundSymbol(name, "Unbound symbol " + name
				    + " - " + ex.toString());
	  }
        try
          {
            rfield = clas.getField(name);
          }
        catch (java.lang.NoSuchFieldException ex)
          {
	    String name = binding.getName();
	    throw new UnboundSymbol(name, "Unbound symbol " + name
				    + " - no field " + name
				    + " in " + type.getName());
          }
      }
  }

  public Object get (Binding binding, Object defaultValue)
  {
    setup(binding);
    try
      {
        return rfield.get(getValue(binding));
      }
    catch (IllegalAccessException ex)
      {
        throw new WrappedException(ex);
      }
  }

  public void set (Binding binding, Object value)
  {
    setup(binding);
    try
      {
        rfield.set(getValue(binding), value);
	return;
      }
    catch (IllegalAccessException ex)
      {
      }
    // This is a bit of a kludge  FIXME.
    setConstraint(binding, new TrivialConstraint(getEnvironment(binding)));
    setValue(binding, value);
  }

  public static void define (String name, Object object, String fname)
  {
    define(name, object, fname, Environment.getCurrent());
  }

  public static void define (String name, Object object, String fname,
                             Environment env)
  {
    Binding binding = env.getBinding(name);
    synchronized (binding)
      {
	setValue(binding, object);
	setConstraint(binding,
                      new ClassMemberConstraint(object.getClass(), fname));
      }
  }

  public static void define (String name, Object object,
                             java.lang.reflect.Field field, Environment env)
  {
    if ((field.getModifiers() & java.lang.reflect.Modifier.FINAL) != 0)
      {
	try
	  {
	    Object value = field.get(object);
	    if (value instanceof Binding)
	      {
		env.addBinding((Binding) value);
		return;
	      }
	    String vname
	      = value instanceof Named ? ((Named) value).getName() : null;
	    name = (vname != null ? vname
		    : Compilation.demangleName(name, true));

	    // The problem with the following is that we can't catch
	    // set! to a constant (defined using define-contsant).  (Note we
	    // do want to allow a new define, at leastwhen interactive.)
	    // However, if we always use a ClassMemberConstraint then
	    // some hitherto-valid Scheme programs will break:  Since it
	    // will also prohibit re-assigning to a procedure defined
	    // using (define (foo ...) ...) since that gets compiled to a
	    // final Procedure field.  FIXME.
	    if (true)
	      {
		name = name.intern();
		env.define(name, value);
		return;
	      }
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException("error accessing field "+field, ex);
	  }
      }
    name = name.intern();
    Binding binding = new Binding(name);
    setValue(binding, object);
    setConstraint(binding, new ClassMemberConstraint(field));
    env.addBinding(binding);
  }

  /** Import all the public fields of an object. */
  public static void defineAll(Object object, Environment env)
  {
    Class clas = object.getClass();
    java.lang.reflect.Field[] fields = clas.getFields();
    for (int i = fields.length;  --i >= 0; )
      {
	java.lang.reflect.Field field = fields[i];
	define(field.getName(), object, field, env);
      }
  }
}
