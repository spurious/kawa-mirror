// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location that forwards to a thread-specific Location.
 */

public class ThreadLocation extends Location
{
  final Symbol name;
  final Object property;
  static int counter;
  /** Used as a "property" key for an anonymous fluid binding. */
  static final Object DYNAMIC = new Symbol("(dynamic)", null);
  private static synchronized int nextCounter() { return ++counter; }

  Location global;

  /* #ifdef JAVA2 */
  ThreadLocal thLocal = new ThreadLocal();
  /* #else */
  // static java.util.Hashtable threadMap = new java.util.Hashtable(50);
  /* #endif */

  /** A new anonymous fluid location. */
  public ThreadLocation ()
  {
    this(new Symbol("param#"+nextCounter()));
  }

  /** A new anonymous fluid location but used a given name for printing.
   * However, the binding is not bound to the name as a visible binding. */
  public ThreadLocation (Symbol name)
  {
    this.name = name;
    this.property = DYNAMIC;
  }

  public ThreadLocation (Symbol name, Object property, Location global)
  {
    this.name = name;
    this.property = property;
    this.global = global;
  }

  /** Set the default/global value. */
  public void setGlobal (Object value)
  {
    global.set(value);
  }

  /** Get the thread-specific Location for this Location. */
  public Location getLocation ()
  {
    Object entry;
    /* #ifdef JAVA2 */
    entry = (Location) thLocal.get();
    /* #else */
    // entry = threadMap.get(Thread.currentThread());
    /* #endif */
    if (entry == null)
      {
	Environment env = Environment.getCurrent();
	Location loc = env.getLocation(name, property);
	if (global != null && loc instanceof IndirectableLocation)
	  {
	    IndirectableLocation iloc = (IndirectableLocation) loc;
	    synchronized (iloc)
	      {
		if (iloc.base == null && iloc.value == Location.UNBOUND)
		  iloc.setBase(global);
	      }
	  }
	
	if (property == DYNAMIC)
	  {
	    LocationRef lref = new LocationRef();
	    lref.sym = name;
	    lref.env = env;
	    lref.loc = loc;
	    entry = lref;
	  }
	else
	  entry = loc;
	/* #ifdef JAVA2 */
	thLocal.set(entry);
	/* #else */
	// threadMap.put(Thread.currentThread(), entry);
	/* #endif */
      }
    if (entry instanceof LocationRef)
      return ((LocationRef) entry).loc;
    else
      return (Location) entry;
  }

  public Object get (Object defaultValue)
  {
    return getLocation().get(defaultValue);
  }

  public void set (Object value)
  {
    getLocation().set(value);
  }

  public Object setWithSave (Object newValue)
  {
    return getLocation().setWithSave(newValue);
  }

  public void setRestore (Object oldValue)
  {
    getLocation().setRestore(oldValue);
  }

  public Symbol getKeySymbol () { return name; }
  public Object getKeyProperty () { return property; }

  static SimpleEnvironment env;

  /** For a given (Symbol. property)-pair, find or create
   * a matching ThreadLocation. */
  public synchronized static ThreadLocation
  getInstance(Symbol name, Object property)
  {
    if (env == null)
      env = new SimpleEnvironment("[thread-locations]");
    IndirectableLocation loc
      = (IndirectableLocation) env.getLocation(name, property);
    if (loc.base != null)
      return (ThreadLocation) loc.base;
    ThreadLocation tloc = new ThreadLocation(name, property, null);
    loc.base = tloc;
    return tloc;
  }
}

class LocationRef
{
  Environment env;
  Location loc;
  Symbol sym;

  public void finalize ()
  {
    env.remove(sym, ThreadLocation.DYNAMIC);
  }
}
