package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.Type;

/** A Constraint whose value is that of a named field/method of an object.
 * The object used is the owning Binding's value.
 * (For now, only fields are supported.)
 */

public class ClassMemberConstraint extends Constraint
{
  Type type;
  String name;
  gnu.bytecode.Field field;
  java.lang.reflect.Field rfield;

  public ClassMemberConstraint(Type type, String name)
  {
    this.type = type;
    this.name = name;
  }

  public ClassMemberConstraint(Class clas, String name)
  {
    this.type = Type.make(clas);
    this.name = name;
  }

  public ClassMemberConstraint(Type type, gnu.bytecode.Field field)
  {
    this.type = type;
    this.field = field;
    this.name = field.getName();
  }

  void setup()
  {
    if (rfield == null)
      {
        Class clas = type.getReflectClass();
        try
          {
            rfield = clas.getField(name);
          }
        catch (java.lang.NoSuchFieldException ex)
          {
          }
      }
  }

  public Object get (Binding binding)
  {
    setup();
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
    setup();
    try
      {
        rfield.set(getValue(binding), value);
      }
    catch (IllegalAccessException ex)
      {
        throw new WrappedException(ex);
      }
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
                             gnu.bytecode.Field field, Environment env)
  {
    Binding binding = env.getBinding(name);
    synchronized (binding)
      {
	setValue(binding, object);
	setConstraint(binding,
                      new ClassMemberConstraint(field.getType(), field));
      }
  }

}
