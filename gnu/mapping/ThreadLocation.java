// Copyright (c) 2005, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location that forwards to a thread-specific Location.
 */

public class ThreadLocation<T> extends NamedLocation<T> implements Named
{
  static int counter;
  private static synchronized int nextCounter() { return ++counter; }

  /** Magic property value used for the "anonymous" ThreadLocations.
   * These are thread-specific dynamic "parameters" (in the SRFI-39 sense)
   * that are not tied to a specfic name. */
  public static final String ANONYMOUS = new String("(dynamic)");

  static final Object NULL_PROXY = new Object();

  // Only used when property!=ANONYMOUS.
  SharedLocation<T> global;

  // Only used when property==ANONYMOUS.
  private ThreadLocal thLocal;

  // Only used as a hashcode when property!=ANONYMOUS.
  // Used for importedThreadLocal when property==ANONYMOUS.
  private int hash;

  private boolean importedThreadLocal() { return hash < 0; }


  /** A new anonymous fluid location. */
  public ThreadLocation ()
  {
    this("param#"+nextCounter());
  }

    /** A new anonymous fluid location but used a given name for printing.
     * However, the binding is not bound to the name as a visible binding. */
    public ThreadLocation(String name) {
        this(Symbol.makeUninterned(name));
    }

    public ThreadLocation(Symbol name) {
        super(name, ANONYMOUS);
        thLocal = new ThreadLocalWithDefault<Object>(null);
    }

    public ThreadLocation(Symbol name, ThreadLocal<T> thLocal) {
        super(name, ANONYMOUS);
        this.thLocal = thLocal;
        hash = -1; // set importedThreadLocal() to true.
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
    public void setGlobal(T value) {
        synchronized (this) {
            if (property == ANONYMOUS) {
                ((ThreadLocalWithDefault) thLocal).setDefault(value);
            } else {
                if (global == null)
                    global = new SharedLocation<T>(this.name, null, 0);
                global.set(value);
            }
        }
    }

    /** Get the thread-specific Location for this Location. */
    public NamedLocation<T> getLocation() {
        if (property == ANONYMOUS)
            throw new Error("getLocation not allowed for ANONYMOUS ThreadLocation");
        return Environment.getCurrent().getLocation(name, property, hash, true);
    }

    public T get() {
        if (property != ANONYMOUS)
            return getLocation().get();
        Object value = thLocal.get();
        if (importedThreadLocal())
            return (T) value;
        if (value == Location.UNBOUND)
            throw new UnboundLocationException(this);
        return value == NULL_PROXY ? null : (T) value;
    }

    public T get(T defaultValue) {
        if (property != ANONYMOUS)
            return getLocation().get(defaultValue);
        Object value = thLocal.get();
        if (importedThreadLocal())
            return (T) value;
        if (value == UNBOUND)
            return defaultValue;
        return value == NULL_PROXY ? null : (T) value;
    }

    public boolean isBound() {
        if (property != ANONYMOUS)
            return getLocation().isBound();
        return importedThreadLocal() || thLocal.get() != Location.UNBOUND;
    }

    public void set(T value) {
        if (property != ANONYMOUS)
            getLocation().set(value);
        else
            thLocal.set(value == null && ! importedThreadLocal() ? NULL_PROXY
                        : value);
    }

    public Object setWithSave (T newValue) {
        if (property != ANONYMOUS)
            return getLocation().setWithSave(newValue);
        Object old = thLocal.get();
        // Don't inline set, since set may be overridden.
        set(newValue);
        return old;
    }

    public void setRestore(Object oldValue) {
        if (property != ANONYMOUS)
            getLocation().setRestore(oldValue);
        else
            thLocal.set(oldValue);
    }

    public void undefine() {
        if (property != ANONYMOUS)
            getLocation().undefine();
        else if (importedThreadLocal())
            thLocal.remove();
        else
            thLocal.set(UNBOUND); // FIXME - maybe use remove?
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

    static class ThreadLocalWithDefault<T> extends InheritableThreadLocal<T> {
        T defaultValue;

        public ThreadLocalWithDefault(T defaultValue) {
            this.defaultValue = defaultValue;
        }

        public void setDefault(T value) { defaultValue = value; }

        protected T initialValue() { return defaultValue; }
    }
}
