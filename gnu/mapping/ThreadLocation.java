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
  private static synchronized int nextCounter() { return ++counter; }

  Location global;

  /* #ifdef JAVA2 */
  ThreadLocal thLocal = new ThreadLocal();
  /* #else */
  // static java.util.Hashtable threadMap = new java.util.Hashtable(50);
  /* #endif */

  public ThreadLocation ()
  {
    this.name = new Symbol("param#"+nextCounter());
    this.property = this;
    this.global = new SharedLocation(this.name, null, 0);
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
    Location loc;
    /* #ifdef JAVA2 */
    loc = (Location) thLocal.get();
    /* #else */
    // loc = threadMap.get(Thread.currentThread());
    /* #endif */
    if (loc == null)
      {
	Environment env = Environment.getCurrent();
	loc = env.getLocation(name, property);
	if (global != null && loc instanceof IndirectableLocation)
	  {
	    IndirectableLocation iloc = (IndirectableLocation) loc;
	    synchronized (iloc)
	      {
		if (iloc.base == null && iloc.value == Location.UNBOUND)
		  iloc.setBase(global);
	      }
	  }
	/* #ifdef JAVA2 */
	thLocal.set(loc);
	/* #else */
	// threadMap.put(Thread.currentThread(), loc);
	/* #endif */
      }
    return loc;
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
