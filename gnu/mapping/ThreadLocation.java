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
  boolean unlink;
  private static synchronized int nextCounter() { return ++counter; }

  /** Magic property value used for the "anonymous" ThreadLocations.
   * These are thread-specific dynamic "parameters" (in the SRFI-39 sense)
   * that are not tied to a specfic name. */
  public static final String ANONYMOUS = new String("(dynamic)");

  Location global;

  /* #ifdef JAVA2 */
  ThreadLocal thLocal = new ThreadLocal();
  /* #else */
  // java.util.Hashtable threadMap = new java.util.Hashtable(50);
  /* #endif */

  /** A new anonymous fluid location. */
  public ThreadLocation ()
  {
    this("param#"+nextCounter());
  }

  /** A new anonymous fluid location but used a given name for printing.
   * However, the binding is not bound to the name as a visible binding. */
  public ThreadLocation (String name)
  {
    this.name = new Symbol(name);
    this.property = ANONYMOUS;
    unlink = true;
  }

  public ThreadLocation (Symbol name, Object property, Location global)
  {
    this.name = name;
    this.property = property;
    this.global = global;
  }

  /** Create a fresh ThreadLocation, independent of other ThreaDLocations.
   * Creates new unique EnvironmentKey, using a unique property key.
   * @param name used for printing, but not identification.
   */
  public static ThreadLocation makePrivate (String name)
  {
    return new ThreadLocation(name);
  }

  /** Set the default/global value. */
  public void setGlobal (Object value)
  {
    synchronized (this)
      {
	if (global == null)
	  global = new SharedLocation(this.name, null, 0);
	global.set(value);
      }
  }

  /** Get the thread-specific Location for this Location. */
  public Location getLocation ()
  {
    Object entry;
    /* #ifdef JAVA2 */
    entry = thLocal.get();
    /* #else */
    // entry = threadMap.get(Thread.currentThread());
    /* #endif */
    if (entry == null)
      {
	Environment env = Environment.getCurrent();
	NamedLocation loc = env.getLocation(name, property, true);
	if (global != null)
	  {
	    synchronized (loc)
	      {
		if (loc.base == null && loc.value == Location.UNBOUND)
		  loc.setBase(global);
	      }
	  }
	
	if (unlink)
	  {
	    LocationRef lref = new LocationRef();
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

  public void finalize ()
  {
    env.remove(loc.getKeySymbol(), loc.getKeyProperty());
  }
}
