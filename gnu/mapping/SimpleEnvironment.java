// Copyright (c) 1996-2000, 2001, 2002, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.io.*;

/** Concrete implementation of <code>Environment</code>.
 */

public class SimpleEnvironment extends Environment
{
  NamedLocation[] table;
  int log2Size;
  private int mask;
  /** Doesn't count inherited bindings. */
  int num_bindings;
  int currentTimestamp;

  /* #ifdef JAVA2 */
  java.lang.ref.ReferenceQueue queue = new java.lang.ref.ReferenceQueue();
  /* #endif */

  /** Size does not include inherited Locations. */
  public int size () { return num_bindings; }

  public static Location getCurrentLocation (String name)
  {
    return getCurrent().getLocation(name, true);
  }

  public static Object lookup_global (Symbol name)
       throws UnboundLocationException
  {
    Location binding = getCurrent().lookup(name);
    if (binding == null)
      throw new UnboundLocationException(name);
    return binding.get();
  }

  /** A special "end-of-list" value added for the sake of getEnvironment. */
  NamedLocation sharedTail;

  public SimpleEnvironment ()
  {
    this(64);
  }

  public SimpleEnvironment (String name)
  {
    this();
    setName(name);
  }

  public SimpleEnvironment (int capacity)
  {
    log2Size = 4;
    while (capacity > (1 << log2Size))
      log2Size++;
    capacity = 1 << log2Size;
    table = new NamedLocation[capacity];
    mask = capacity - 1;

    sharedTail = new PlainLocation(null, null, this);
  }

  public NamedLocation lookup (Symbol name, Object property, int hash)
  {
    return lookupDirect(name, property, hash);
  }

  public NamedLocation lookupDirect (Symbol name, Object property, int hash)
  {
    int index = hash & this.mask;
    for (NamedLocation loc = table[index];
	 loc != null;  loc = loc.next)
      {
	if (loc.matches(name, property))
	  return loc;
      }
    return null;
  }

  public synchronized NamedLocation
  getLocation (Symbol name, Object property, boolean create)
  {
    int hash = name.hashCode() ^ System.identityHashCode(property);
    NamedLocation loc = lookup(name, property, hash);
    if (loc != null)
      return loc;
    if (! create)
      return null;
    return addUnboundLocation(name, property, hash);
  }

  protected NamedLocation addUnboundLocation(Symbol name, Object property,
					     int hash)
  {
    int index = hash & mask;
    NamedLocation loc = newEntry(name, property, index);
    loc.base = null;
    loc.value = Location.UNBOUND;
    return loc;
  }

  public void put(Symbol key, Object property, Object newValue)
  {
    boolean create = (flags & CAN_IMPLICITLY_DEFINE) != 0;
    Location loc = getLocation(key, property, create);
    if (loc == null)
      throw new UnboundLocationException(key);
    else if (loc.isConstant())
      throw new IllegalStateException("attempt to modify read-only location: "
				      + key + " in "+this+" loc:"+loc);
    loc.set(newValue);
  }

  // Rename to newEntry? FIXME
  NamedLocation newLocation (Symbol name, Object property)
  {
    if ((flags & THREAD_SAFE) != 0)
      return new SharedLocation(name, property, currentTimestamp);
    else
      return new PlainLocation(name, property);
  }

  NamedLocation newEntry (Symbol name, Object property, int index)
  {
    NamedLocation loc = newLocation(name, property);
    NamedLocation first = table[index];
    loc.next = first == null ? sharedTail : first;
    table[index] = loc;
    num_bindings++;
    if (num_bindings >= table.length)
      rehash();
    return loc;
  }

  public NamedLocation define (Symbol sym, Object property, int hash,
			       Object newValue)
  {
    int index = hash & mask;
    NamedLocation prev = null;
    NamedLocation first = table[index];
    NamedLocation loc = first;
    for (;;)
      {
	if (loc == null)
	  {
	    // FIXME increment numBindings?
	    loc = newEntry(sym, property, index);
	    loc.set(newValue);
	    return loc;
	  }
	else if (loc.matches(sym, property))
	  {
	    if (! (loc.isBound() ? getCanDefine() : getCanRedefine()))
	      redefineError(sym, property, loc);
	    loc.base = null;
	    loc.value = newValue;
	    return loc;
	  }
	prev = loc;
	loc = loc.next;
      }
  }

  public void define (Symbol sym, Object property, Object newValue)
  {
    int hash = sym.hashCode() ^ System.identityHashCode(property);
    define(sym, property, hash, newValue);
  }

  protected void redefineError (Symbol name, Object property, Location loc)
  {
    throw new IllegalStateException("prohibited define/redefine of "
                                    +name+" in "+this);
  }

  public NamedLocation addLocation (Symbol name, Object property, Location loc)
  {
    return addLocation(name, property,
		       name.hashCode() ^ System.identityHashCode(property), loc);
  }

  // FIXME rename to define
  NamedLocation addLocation (Symbol name, Object property, int hash, Location loc)
  {
    if (loc instanceof ThreadLocation)
      loc = ((ThreadLocation) loc).getLocation();
    NamedLocation nloc = lookupDirect(name, property, hash);
    if (loc == nloc)
      return nloc;
    boolean bound = (nloc != null);
    if (! bound)
      nloc = addUnboundLocation(name, property, hash);
    if ((flags & CAN_DEFINE+CAN_REDEFINE) != CAN_DEFINE+CAN_REDEFINE)
      {
	if (bound)
	  bound = nloc.isBound();
	// We do this redundant testing to avoid invoking isBound,
	// which may be expensive and/or cause needless errors, such in the
	// case of a lazy ClassMemberLocation referring to a missing class.
	if (bound
	    ? ((flags & CAN_REDEFINE) == 0)
	    : ((flags & CAN_DEFINE) == 0) && loc.isBound());
	  redefineError(name, property, nloc);
      }
    nloc.base = loc;
    nloc.value = IndirectableLocation.INDIRECT_FLUIDS;
    return nloc;
  }

  void rehash ()
  {
    NamedLocation[] oldTable = table;
    int oldCapacity = oldTable.length;
    int newCapacity = 2 * oldCapacity;
    NamedLocation[] newTable = new NamedLocation[newCapacity];
    int newMask = newCapacity - 1;
    for (int i = oldCapacity;  --i >= 0;)
      {
	for (NamedLocation element = oldTable[i];
	     element != null && element != sharedTail;  )
	  {
	    NamedLocation next = element.next;
	    Symbol name = element.name;
	    Object property = element.property;
	    int hash = name.hashCode() ^ System.identityHashCode(property);
	    int j = hash & newMask;
	    NamedLocation head = newTable[j];
	    if (head == null)
	      head = sharedTail;
	    element.next = head;
	    newTable[j] = element;
	    element = next;
	  }
      }
    table = newTable;
    log2Size++;
    mask = newMask;
  }

  public void remove (Symbol symbol, Object property)
  {
    int index = ((symbol.hashCode() ^ System.identityHashCode(property))
		 & this.mask);
    NamedLocation prev = null;
    NamedLocation loc = table[index];
    while (loc != null)
      {
	NamedLocation next = loc.next;
	if (loc.matches(symbol, property))
	  {
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.next = loc;
	    num_bindings--;
	    if (loc instanceof IndirectableLocation)
	      ((IndirectableLocation) loc).undefine();
	    return;
	  }
	prev = loc;
	loc = next;
      }
  }

  public Object remove (EnvironmentKey key)
  {
    Symbol symbol = key.getKeySymbol();
    Object property = key.getKeyProperty();
    int index = ((symbol.hashCode() ^ System.identityHashCode(property))
		 & this.mask);
    NamedLocation prev = null;
    NamedLocation loc = table[index];
    while (loc != null)
      {
	NamedLocation next = loc.next;
	if (loc.matches(symbol, property))
	  {
	    Object value = loc.get(null);
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.next = loc;
	    num_bindings--;
	    if (loc instanceof IndirectableLocation)
	      ((IndirectableLocation) loc).undefine();
	    return value;
	  }
	prev = loc;
	loc = next;
      }
    return null;
  }

  /** Does not enumerate inherited Locations. */
  public LocationEnumeration enumerateLocations()
  {
    LocationEnumeration it = new LocationEnumeration(table, 1 << log2Size);
    it.env = this;
    return it;
  }

  /** Does enumerate inherited Locations. */
  public LocationEnumeration enumerateAllLocations()
  {
    return enumerateLocations();
  }

  protected boolean hasMoreElements (LocationEnumeration it)
  {
    for (;;)
      {
	if (it.curLoc == null)
	  {
	    if (--it.index < 0)
	      return false;
	    it.curLoc = it.bindings[it.index];
	    if (it.curLoc == null)
	      continue;
	  }
	if (it.curLoc.name == null)
	  it.curLoc = it.curLoc.next;
	else
	  break;
      }
    return true;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getSymbol());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setSymbol(in.readObject());
  }

  public Object readResolve() throws ObjectStreamException
  {
    String name = getName();
    Environment env = (Environment) envTable.get(name);
    if (env != null)
      return env;
    envTable.put(name, this);
    return this;
   
  }

  public synchronized void makeShared ()
  {
    if ((flags & THREAD_SAFE) != 0)
      return;
    flags |= THREAD_SAFE;
    for (int index = table.length;  --index >= 0; )
      {
	NamedLocation prev = null;
	int timestamp = currentTimestamp;
	for (NamedLocation loc = table[index];  loc != null; )
	  {
	    NamedLocation next = loc.next;
	    if (! (loc instanceof SharedLocation) && loc != sharedTail)
	      {
		SharedLocation sloc;
		sloc = new SharedLocation(loc.name, loc.property, timestamp);
		sloc.base = loc.base;
		sloc.value = loc.value;
		loc.next = null;
		loc.base = sloc;
		loc.value = IndirectableLocation.INDIRECT_FLUIDS;
		loc = sloc;
	      }
	    if (prev == null)
	      table[index] = loc;
	    else
	      prev.next = loc;
	    prev = loc;
	    loc = next;
	  }
      }
  }

  /* #ifdef JAVA2 */
  public java.util.Set entrySet ()
  {
    return new EnvironmentMappings(this);
  }
  /* #endif */

  public String toStringVerbose ()
  {
    StringBuffer sbuf = new StringBuffer();
    toStringBase(sbuf);
    return "#<environment "+getName()+" num:"+num_bindings
      +" ts:"+currentTimestamp+sbuf+'>';
  }

  protected void toStringBase (StringBuffer sbuf) { ; }
}

/* #ifdef JAVA2 */
class EnvironmentMappings
  extends java.util.AbstractSet /* <Location> */
{
  SimpleEnvironment env;

  public EnvironmentMappings (SimpleEnvironment env) { this.env = env; }

  public int size() { return env.size(); }

  public java.util.Iterator iterator ()
  {
    return new LocationEnumeration(env);
  }
}
/* #endif */
