// Copyright (c) 2005, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location that forwards to a thread-specific Location.
 */

public class DynamicLocation<T> extends NamedLocation<T> implements Named
{
    SharedLocation<T> global;

    private int hash;

    static SimpleEnvironment env;

    public DynamicLocation (Symbol name, Object property, SharedLocation global)  {
        super(name, property);
        hash = name.hashCode() ^ System.identityHashCode(property);
        this.global = global;
    }

    /** Set the default/global value. */
    public void setGlobal(T value) {
        synchronized (this) {
            if (global == null)
                global = new SharedLocation<T>(this.name, null, 0);
            global.set(value);
        }
    }

    /** Get the thread-specific Location for this Location. */
    public NamedLocation<T> getLocation() {
        Environment curenv = Environment.getCurrent();
        NamedLocation<T> loc =  curenv.getLocation(name, property, hash, true);
        return loc;
    }

    public T get() {
        return getLocation().get();
    }

    public T get(T defaultValue) {
        return getLocation().get(defaultValue);
    }

    public boolean isBound() {
        return getLocation().isBound();
    }

    public void set(T value) {
        getLocation().set(value);
    }

    public Object setWithSave (T newValue) {
        return getLocation().setWithSave(newValue);
    }

    public void setRestore(Object oldValue) {
        getLocation().setRestore(oldValue);
    }

    public void undefine() {
        getLocation().undefine();
    }

    public String getName() { return name == null ? null : name.toString(); }
    public Object getSymbol() { // Implements Named
        return name;
    }
    public void setName(String name)
    { throw new RuntimeException("setName not allowed"); }

    /** For a given (Symbol. property)-pair, find or create
     * a matching DynamicLocation.
     */
    public synchronized static
    DynamicLocation getInstance(Symbol name, Object property) {
        if (env == null)
            env = new SimpleEnvironment("[thread-locations]");
        IndirectableLocation loc
            = (IndirectableLocation) env.getLocation(name, property);
        if (loc.base != null)
            return (DynamicLocation) loc.base;
        DynamicLocation tloc = new DynamicLocation(name, property, null);
        loc.base = tloc;
        return tloc;
    }
}

