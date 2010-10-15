// Copyright (c) 2005, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location that forwards to a thread-specific Location.
 */

public class ThreadLocation extends NamedLocation implements Named
{
  static int counter;
  private static synchronized int nextCounter() { return ++counter; }

  /** Magic property value used for the "anonymous" ThreadLocations.
   * These are thread-specific dynamic "parameters" (in the SRFI-39 sense)
   * that are not tied to a specfic name. */
  public static final String ANONYMOUS = new String("(dynamic)");

  SharedLocation global;

  // Only used when property==ANONYMOUS.
  private ThreadLocal<NamedLocation> thLocal;

  // Only used when property!=ANONYMOUS.
  private int hash;


  /** A new anonymous fluid location. */
  public ThreadLocation ()
  {
    this("param#"+nextCounter());
  }

  /** A new anonymous fluid location but used a given name for printing.
   * However, the binding is not bound to the name as a visible binding. */
  public ThreadLocation (String name)
  {
    super(Symbol.makeUninterned(name), ANONYMOUS);
    thLocal = new InheritingLocation();
    global = new SharedLocation(this.name, null, 0);
  }

  private ThreadLocation (Symbol name)
  {
    super(name, ANONYMOUS);
    thLocal = new InheritingLocation();
    String str = name == null ? null : name.toString();
    global = new SharedLocation(Symbol.makeUninterned(str), null, 0);
  }

  public ThreadLocation (Symbol name, Object property, SharedLocation global)
  {
    super(name, property);
    hash = name.hashCode() ^ System.identityHashCode(property);
    this.global = global;
  }

  /** Create a fresh ThreadLocation, independent of other ThreadLocations.
   * @param name used for printing, but not identification.
   */
  public static ThreadLocation makeAnonymous (String name)
  {
    return new ThreadLocation(name);
  }

  /** Create a fresh ThreadLocation, independent of other ThreadLocations.
   * @param name used for printing, but not identification.
   */
  public static ThreadLocation makeAnonymous (Symbol name)
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
  public NamedLocation getLocation ()
  {
    if (property != ANONYMOUS)
      {
        return Environment.getCurrent().getLocation(name, property, hash, true);
      }
    NamedLocation entry = (NamedLocation) thLocal.get();
    if (entry == null)
      {
        entry = new SharedLocation(name, property, 0);
        if (global != null)
          entry.setBase(global);
	thLocal.set(entry);
      }
    return entry;
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

  public String getName () { return name == null ? null : name.toString(); }
  public Object getSymbol () // Implements Named
  {
    // If this was allocated using makeAnonymous(String) it is better
    // to return the original String, rather than a generated Symbol.
    // One motivation is when a module is imported with a specified namespace
    // URI (only in XQuery at this point); we want to use the latter namespace.
    if (name != null && property == ANONYMOUS
        && (((SharedLocation) global).getKeySymbol() == name))
      return name.toString();
    return name;
  }
  public void setName (String name)
  { throw new RuntimeException("setName not allowed"); }

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

  public class InheritingLocation
    extends InheritableThreadLocal<NamedLocation>
  {
    protected SharedLocation childValue(NamedLocation parentValue)
    {
      if (property != ANONYMOUS)
        throw new Error();
      if (parentValue == null)
        parentValue = (SharedLocation) getLocation();
      NamedLocation nloc = parentValue;
      if (nloc.base == null)
        {
          SharedLocation sloc = new SharedLocation(name, property, 0);
          sloc.value = nloc.value;
          nloc.base = sloc;
          nloc.value = null;
          nloc = sloc;
        }
      SharedLocation sloc = new SharedLocation(name, property, 0);
      sloc.value = null;
      sloc.base = nloc;
      return sloc;
    }
  }
}
