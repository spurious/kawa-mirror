package gnu.mapping;

/** A Binding is a Location in an Environment object. */

public final class Binding extends Location
    // implements java.util.Map.Entry
{
  /** The current value of the binding. */
  Object value;

  Constraint constraint;

  public final Object get ()
  {
    return constraint.get(this);
  }

  public final Procedure getProcedure ()
  {
    return constraint.getProcedure(this);
  }

  public final void set (Object value)
  {
    constraint.set(this, value);
  }

  public final void setConstraint (Constraint constraint)
  {
    this.constraint = constraint;
  }

  public boolean isBound ()
  {
    return ! (constraint instanceof UnboundConstraint);  // FIXME
  }

  public Binding (String name)
  {
    setName(name); 
  }

  // The compiler emits calls to this method.
  public static Binding make (Object init, String name)
  {
    Binding binding = new Binding(name);
    binding.value = init;
    binding.constraint = new TrivialConstraint(null);
    return binding;
  }

  /** Used to chain multiple Bindings in the same hash bucket.
   * Note that there can be multiple Bindings with the same name;
   * in that case, the newest comes first. */
  Binding chain;

  /** The "time" the binding was created.
   * If the binding is newer than the current thread, it does not count. */
  int time_stamp;

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<binding ");
    String name = getName();
    if (name != null)
      ps.print(name);
    if (isBound())
      {
	ps.print(" -> ");
	SFormat.print(get(), ps);
      }
    else
      ps.print("(unbound)");
    ps.print ('>');
  }

  // Methods that implement java.util.Map.Entry:

  public final Object getKey ()
  {
    return getName();
  }

  public final Object getValue ()
  {
    return constraint.get(this);
  }

  public final Object setValue (Object value)
  {
    Object old = constraint.get(this);
    constraint.set(this, value);
    return old;
  }

  public boolean equals (Object o)
  {
    if (! (o instanceof Binding))
      return false;
    Binding e2 = (Binding) o;
    String e1Key = getName();
    String e2Key = e2.getName();
    // This is quite following the Collections spec, but we assume keys
    // are interned, or if they are not, that they are seen as unequal.
    if (e1Key != e2Key)
      return false;
    Object e1Value = constraint.get(this);
    Object e2Value = e2.constraint.get(e2);
    return e1Value == null ? e2Value == null : e1Value.equals(e2Value);
  }

  public int hashCode ()
  {
    // Note:  The hashCode should not depend on the value.
    return System.identityHashCode(this); // ^ System.identityHashCode(env);
  }
}
