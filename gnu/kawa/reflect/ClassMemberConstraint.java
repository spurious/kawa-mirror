package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.expr.Compilation;

/** A Constraint whose value is that of a named field/method of an object.
 * The object is used as the owning Symbol's value.
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

  void setup(Symbol symbol)
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
	    String bname = symbol.getName();
	    throw new UnboundSymbol(bname, "Unbound symbol " + bname
				    + " - " + ex.toString());
	  }
        try
          {
            rfield = clas.getField(name);
          }
        catch (java.lang.NoSuchFieldException ex)
          {
	    String bname = symbol.getName();
	    throw new UnboundSymbol(bname, "Unbound symbol " + bname
				    + " - no field " + name
				    + " in " + type.getName());
          }
      }
  }

  public Object get (Symbol symbol, Object defaultValue)
  {
    setup(symbol);
    try
      {
        return rfield.get(getValue(symbol));
      }
    catch (IllegalAccessException ex)
      {
        throw new WrappedException(ex);
      }
  }

  public void set (Symbol symbol, Object value)
  {
    setup(symbol);
    try
      {
        rfield.set(getValue(symbol), value);
	return;
      }
    catch (IllegalAccessException ex)
      {
      }
    // This is a bit of a kludge  FIXME.
    setConstraint(symbol, new TrivialConstraint(getEnvironment(symbol)));
    setValue(symbol, value);
  }

  public static void define (String name, Object object, String fname)
  {
    define(name, object, fname, Environment.getCurrent());
  }

  public static void define (String name, Object object, String fname,
                             Environment env)
  {
    Symbol symbol = env.getSymbol(name);
    synchronized (symbol)
      {
	setValue(symbol, object);
	setConstraint(symbol,
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
	    if (field.getType().getName() == "gnu.mapping.Location")
	      { // Handles exported aliases:
		name = Compilation.demangleName(name, true).intern();
		Symbol symbol = env.getSymbol(name);
		AliasConstraint.define(symbol, (Location) value);
		return;
	      }
	    if (value instanceof Symbol)
	      {
		env.addSymbol((Symbol) value);
		return;
	      }
	    String vname
	      = value instanceof Named ? ((Named) value).getName() : null;
	    name = (vname != null ? vname
		    : Compilation.demangleName(name, true));

	    // The problem with the following is that we can't catch
	    // set! to a constant (defined using define-constant).  (Note we
	    // do want to allow a new define, at least when interactive.)
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
    Symbol symbol = new Symbol(name);
    setValue(symbol, object);
    setConstraint(symbol, new ClassMemberConstraint(field));
    env.addSymbol(symbol);
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
