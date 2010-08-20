// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location is an abstract cell/location/variable with a value. */

public abstract class Location
{
  /* DEBUGGING
  static int counter;
  public int id=++counter;
  */

  public Location ()
  {
  }

  public Symbol getKeySymbol ()
  {
    return null;
  }

  public Object getKeyProperty ()
  {
    return null;
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(getClass().getName());
    Symbol sym = getKeySymbol();
    sbuf.append('[');
    if (sym != null)
      {
	sbuf.append(sym);
	Object property = getKeyProperty();
	// For a ThreadLocation the property defaults to "this".
	// In that case we'd better not print the property ...
	if (property != null && property != this)
	  {
	    sbuf.append('/');
	    sbuf.append(property);
	  }
      }
    /* DEBUGGING:
    sbuf.append(" #:");
    sbuf.append(id);
    */
    sbuf.append("]");
    return sbuf.toString();
  }

  /** Magic value used to indicate there is no property binding. */
  public static final String UNBOUND = new String("(unbound)");

  public abstract Object get (Object defaultValue);

  /** Get the current value of this location.
   * @exception UnboundLocationException the location does not have a value. */
  public final Object get ()
  {
    Object unb = Location.UNBOUND;
    Object val = get(unb);
    if (val == unb)
      throw new UnboundLocationException(this);
    return val;
  }

  public abstract void set (Object value);

  public void undefine ()
  {
    set(UNBOUND);
  }

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
    Symbol name = getKeySymbol();
    if (name != null)
      ps.print(name);
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
    ThreadLocation loc = new ThreadLocation(name);
    loc.setGlobal(init);
    return loc;
  }

  // The compiler emits calls to this method.
  public static IndirectableLocation make (String name)
  {
    Symbol sym = Namespace.EmptyNamespace.getSymbol(name.intern());
    PlainLocation loc = new PlainLocation(sym, null);
    loc.base = null;
    loc.value = UNBOUND;
    return loc;
  }

  public static IndirectableLocation make (Symbol name)
  {
    PlainLocation loc = new PlainLocation(name, null);
    loc.base = null;
    loc.value = UNBOUND;
    return loc;
  }
}
