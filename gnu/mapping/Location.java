// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location is an abstract cell/location/variable with a value. */

public abstract class Location
{
  static int counter;
  public int id=++counter;

  public Location ()
  {
  }

  public String toString() { return getClass().getName()+"[#:"+id+"]"; }

  /** Magic value used to indicate there is no property binding. */
  public static final String UNBOUND = new String("(unbound)");

  public abstract Object get (Object defaultValue);

  /** Get the current value of this location.
   * @exception UnboundLocation the location does not have a value. */
  public final Object get ()
  {
    Object unb = Location.UNBOUND;
    Object val = get(unb);
    if (val == unb)
      throw new UnboundLocationException(this);
    return val;
  }

  public abstract void set (Object value);

  /** Set a value, but return cookie so old value can be restored.
   * This is intended for fluid-let where (in the case of multiple threads)
   * a simple save-restore isn't always the right thing. */
  public Object setWithSave (Object newValue)
  {
    Object old = get(UNBOUND);
    set(newValue);
    return old;
  }

  /** Restore an old value.
   * @param oldValue the return value from a prior setWithSave. */
  public void setRestore (Object oldValue)
  {
    // if (oldValue == UNBOUND) ???;  // FIXME
    set(oldValue);
  }

  public boolean isBound ()
  {
    Object unb = Location.UNBOUND;
    return get(unb) != unb;
  }

  public boolean isConstant ()
  {
    return false;
  }

  public Location getBase ()
  {
    return this;
  }

  public final Object getValue ()
  {
    return get(null);
  }

  public final Object setValue (Object newValue)
  {
    Object value = get(null);
    set(newValue);
    return value;
  }

  /** True if directly entered in an Environment.  (Only if NamedLocation.) */
  public boolean entered ()
  {
    return false;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<location ");
    /*
    String name = getName();
    if (name != null)
      ps.print(name);
    */
    Object unb = Location.UNBOUND;
    Object value = get(unb);
    if (value != unb)
      {
	ps.print(" -> ");
	ps.print(value);
      }
    else
      ps.print("(unbound)");
    ps.print ('>');
  }

  // The compiler emits calls to this method.
  public static Location make (Object init, String name)
  {
    Symbol sym = Namespace.EmptyNamespace.getSymbol(name.intern());
    return new PlainLocation(sym, null, init);
  }

  // The compiler emits calls to this method.
  public static IndirectableLocation make (String name)
  {
    Symbol sym = Namespace.EmptyNamespace.getSymbol(name.intern());
    PlainLocation loc = new PlainLocation(sym, null);
    loc.base = UnboundLocation.instance;
    loc.value = IndirectableLocation.DIRECT_ON_SET;
    return loc;
  }

  public static IndirectableLocation make (Symbol name)
  {
    PlainLocation loc = new PlainLocation(name, null);
    loc.base = UnboundLocation.instance;
    loc.value = IndirectableLocation.DIRECT_ON_SET;
    return loc;
  }
}
