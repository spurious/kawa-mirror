package gnu.jemacs.lang;
import gnu.mapping.*;

public abstract class SymbolConstraint extends Constraint
{
  public abstract Object getFunctionBinding (Binding binding);

  public void setFunctionBinding (Binding binding, Object value)
  {
    synchronized (binding)
      {
        Constraint constraint = getConstraint(binding);
        SymbolConstraint sconstraint;
	if (constraint != this && constraint instanceof SymbolConstraint)
          sconstraint = (SymbolConstraint) constraint;
        else
          binding.setConstraint(sconstraint = (new GeneralSymbolConstraint()));
        sconstraint.setFunctionBinding(binding, value);
      }
  }

  public static Object getFunctionBinding (Object symbol)
  {
    Binding binding = Symbol.getBinding(symbol);
    synchronized (binding)
      {
	Constraint constraint = getConstraint(binding);
	SymbolConstraint sconstraint;
	if (constraint instanceof SymbolConstraint)
	  return ((SymbolConstraint) constraint).getFunctionBinding(binding);
	else
	  return constraint.get(binding);
      }
  }

  public static void setFunctionBinding (Environment environ,
					 Object symbol, Object value)
  {
    Binding binding = Symbol.getBinding(environ, symbol);
    synchronized (binding)
      {
	Constraint constraint = getConstraint(binding);
	SymbolConstraint sconstraint;
	if (constraint instanceof SymbolConstraint)
	  sconstraint = (SymbolConstraint) constraint;
	else if (binding.isBound())
	  {
	    sconstraint = new GeneralSymbolConstraint();
	    Object old = constraint.get(binding);
	    binding.setConstraint(sconstraint);
	    sconstraint.set(binding, old);
	  }
	else
	  {
	    sconstraint = new FunctionSymbolConstraint(environ);
	    binding.setConstraint(sconstraint);
	  }
	sconstraint.setFunctionBinding(binding, value);
      }
  }

  public Procedure getProcedure (Binding binding)
  {
    return (Procedure) getFunctionBinding (binding);
  }

  /** Import all the public fields of an object. */
  public static void defineAll(Object object, Environment env)
  {
    Class clas = object.getClass();
    java.lang.reflect.Field[] fields = clas.getFields();
    for (int i = fields.length;  --i >= 0; )
      {
	java.lang.reflect.Field field = fields[i];
	String name = field.getName();
	if ((field.getModifiers() & java.lang.reflect.Modifier.FINAL) != 0)
	  {
	    try
	      {
		Object part = field.get(object);
		if (part instanceof Named)
		  name = ((Named) part).getName();
		else if (part instanceof kawa.lang.Syntax) // FIXME
		  name = ((kawa.lang.Syntax) part).getName();
		else
		  name = name.intern();
		if (part instanceof Binding)
		  env.addBinding((Binding) part);
		else if (part instanceof Procedure
			 || part instanceof kawa.lang.Syntax)
		  SymbolConstraint.setFunctionBinding(env, name, part);
		else
		  env.define(name, part);
	      }
	    catch (Exception ex)
	      {
		throw new WrappedException("error accessing field "+field, ex);
	      }
	  }
	else
	  {
	    Binding binding = new Binding(name);
	    setValue(binding, object);
	    setConstraint(binding, new gnu.kawa.reflect.ClassMemberConstraint(field));
	    env.addBinding(binding);
	  }
      }
  }

}
