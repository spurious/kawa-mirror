// Copyright (c) 1996-2000, 2001, 2002, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.io.*;

/** Concrete implementation of <code>Environment</code>.
 *
 * FIXME: Not correct in terms of GC: We want a Location to be collected
 * when *both* there is no refererence except from Environments *and*
 * the Symbol name *or* property are otherwise unreferenced.
 * See commented-out Entry at end of file for a solution.
 */

public class SimpleEnvironment extends Environment
{
  NamedLocation[] table;
  int log2Size;
  private int mask;
  /** Doesn't count inherited bindings or UnboundLocation. */
  int num_bindings;
  int currentTimestamp;

  /* BEGIN JAVA2 */
  java.lang.ref.ReferenceQueue queue = new java.lang.ref.ReferenceQueue();
  /* END JAVA2 */

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
  UnboundLocation unbound;

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

    unbound = new UnboundLocation(this, null, null);
  }

  public NamedLocation lookup (Symbol name, Object property, int hash)
  {
    int index = hash & this.mask;
    for (NamedLocation loc = table[index];
	 loc != null;  loc = loc.next)
      {
	if (loc.matches(name, property))
	  {
	    if (loc instanceof UnboundLocation)
	      {
		NamedLocation xloc = ((UnboundLocation) loc).getLocation();
		if (xloc != null)
		  return xloc;
	      }
	    else
	      return loc;
	  }
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
    // We don't want any strong references in the GC sense) from the
    // Environment to the Location returned to applications, since we
    // unbound Locations to be collectable.  So the Environment contains
    // an entry for an UnboundLocation, which has a weak reference to the
    // IndirectableLocation that get returned to the client.
    UnboundLocation uloc = new UnboundLocation(this, name, property);
    IndirectableLocation xloc = newLocation(name, property);
    xloc.base = uloc;
    uloc.setLocation(xloc, this);
    int index = hash & mask;
    NamedLocation first = table[index];
    uloc.next = first == null ? unbound : first;
    table[index] = uloc;
    return xloc;
  }

  public void put(Symbol key, Object property, Object newValue)
  {
    Location loc = getLocation(key, property);
    if (loc == null)
      {
	if ((flags & CAN_IMPLICITLY_DEFINE) == 0)
	  throw new UnboundLocationException(key);
	loc = newLocation(key, property);
	addLocation(key, property, loc);
      }
    else if (loc.isConstant())
      {
	if ((flags & CAN_DEFINE) == 0)
	  throw new IllegalStateException("attempt to modify read-only location: "
					  // FIXME cast
					  + ((NamedLocation) loc).getKeySymbol()
					  + " in "+this);

	loc = newLocation(key, property);
	addLocation(key, property, loc);
      }
    loc.set(newValue);
  }

  IndirectableLocation newLocation (Symbol name, Object property)
  {
    if ((flags & THREAD_SAFE) != 0)
      return new SharedLocation(name, property, ++currentTimestamp);
    else
      return new PlainLocation(name, property);
  }

  public NamedLocation define (Symbol sym, Object property, int hash,
			       Object newValue)
  {
    int index = hash & mask;
    NamedLocation prev = null;
    NamedLocation loc = table[index];
    for (;;)
      {
	if (loc == null)
	  {
	    // FIXME - should optimize
	    loc = newLocation(sym, property);
	    loc.set(newValue);
	    addLocation(loc);
	    return loc;
	  }
	else if (loc.matches(sym, property))
	  {
	    if (loc instanceof UnboundLocation)
	      {
		UnboundLocation uloc = (UnboundLocation) loc;
		IndirectableLocation iloc
		  = (IndirectableLocation) uloc.getLocation();
		if (iloc == null)
		  {
		    if (prev == null)
		      table[index] = uloc.next;
		    else
		      prev.next = uloc.next;
		  }
		else
		  {
		    num_bindings++; // ??
		    if (! getCanDefine())
		      redefineError(sym, property, uloc); // ???
		    iloc.next = uloc.next;
		    if (prev == null)
		      table[index] = iloc;
		    else
		      prev.next = iloc;
		    iloc.value = newValue;
		    iloc.base = null;
		    return iloc;
		  }
	      }
	    else
	      {
		if (! getCanRedefine())
		  redefineError(sym, property, loc);
		if (loc instanceof IndirectableLocation)
		  {
		    IndirectableLocation iloc = (IndirectableLocation) loc;
		    iloc.base = null;
		    iloc.value = newValue;
		  }
		else
		  loc.set(newValue);
		return loc;
	      }
	    break;
	  }
	prev = loc;
	loc = loc.next;
      }

    return loc;
  }

  public void define (Symbol sym, Object property, Object newValue)
  {
    int hash = sym.hashCode() ^ System.identityHashCode(property);
    define(sym, property, hash, newValue);
  }

  protected void redefineError (Symbol name, Object property, Location loc)
  {
    throw new IllegalStateException("prohibited define/redefine of "+name);
  }

  public void addLocation (Symbol name, Object property, Location loc)
  {
    addLocation(name, property,
		name.hashCode() ^ System.identityHashCode(property), loc);
  }

  void addLocation (Symbol name, Object property, int hash, Location loc)
  {
    int index = hash & mask;
    NamedLocation prev = null;
    NamedLocation first = table[index];
    NamedLocation old = first;
    for (; old != null && ! old.matches(name, property);  old = old.next)
      prev = old;

    if (! getCanDefine()
	|| ! (getCanRedefine() || old instanceof UnboundLocation))
      redefineError(name, property, loc);

    NamedLocation nloc;
    if (old instanceof IndirectableLocation)
      ((IndirectableLocation) old).base = loc;
    else if (old instanceof UnboundLocation)
      {
	nloc = ((UnboundLocation) old).getLocation();
	if (nloc instanceof IndirectableLocation)
	  ((IndirectableLocation)  nloc).base = loc;
      }

    if (loc.entered()
	|| (getCanRedefine() && ! (loc instanceof IndirectableLocation))
	|| ! (loc instanceof NamedLocation)
	|| (nloc = (NamedLocation) loc).name != name
	|| nloc.property != property)
      {
	if (old instanceof IndirectableLocation)
	  return;
	IndirectableLocation xloc = newLocation(name, property);

	xloc.base = loc;
	nloc = xloc;
      }

    if (old == null)
      {
	nloc.next = first == null ? unbound : first;
	table[index] = nloc;
      }
    else if (prev == null)
      {
	first = first.next;
	nloc.next = first == null ? unbound : first;
	table[index] = nloc;
      }
    else
      {
	nloc.next = old.next;
	prev.next = nloc;
      }
    if (old == null || old instanceof UnboundLocation)
      num_bindings++;
    if (num_bindings >= table.length) // FIXME
      rehash();
  }

  void rehash ()
  {
    NamedLocation[] oldTable = table;
    int oldCapacity = oldTable.length;
    int newCapacity = 2 * oldCapacity;
    NamedLocation[] newTable = new NamedLocation[newCapacity];
    int newMask = newCapacity - 1;
    int countInserted = 0;
    for (int i = oldCapacity;  --i >= 0;)
      {
	for (NamedLocation element = oldTable[i];
	     element != null && element != unbound;  )
	  {
	    NamedLocation next = element.next;
	    Symbol name = element.name;
	    Object property = element.property;
	    int hash = name.hashCode() ^ System.identityHashCode(property);
	    int j = hash & newMask;
	    NamedLocation head = newTable[j];
	    if (head == null)
	      head = unbound;
	    element.next = head;
	    newTable[j] = element;
	    if (! (element instanceof UnboundLocation))
	      countInserted++;
	    element = next;
	  }
      }
    table = newTable;
    log2Size++;
    mask = newMask;
    num_bindings = countInserted;
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
	if (it.curLoc.name == null
	    || (it.curLoc instanceof UnboundLocation
		&& ((UnboundLocation) it.curLoc).getLocation() == null))
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
	int timestamp = ++currentTimestamp;
	for (NamedLocation loc = table[index];  loc != null;
	     prev = loc, loc = loc.next)
	  {
	    if (! (loc instanceof SharedLocation) && loc != unbound)
	      {
		SharedLocation sloc;
		sloc = new SharedLocation(loc.name, loc.property, timestamp);
		Location oldBase = loc.getBase();
		if (oldBase instanceof IndirectableLocation)
		  {
		    IndirectableLocation iloc = (IndirectableLocation) oldBase;
		    sloc.value = iloc.value;
		    iloc.value = null;
		    iloc.base = sloc;
		  }
		else
		  sloc.base = loc;
		loc = sloc;
	      }
	    if (prev == null)
	      table[index] = loc;
	    else
	      prev.next = loc;
	  }
      }
  }

  /* BEGIN JAVA2 */
  public java.util.Set entrySet ()
  {
    return new EnvironmentMappings(this);
  }
  /* END JAVA2 */
}

/* BEGIN JAVA2 */
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
/* END JAVA2 */

/* * A possible (but not low-overhead) solution for GC problem
 * mentioned at top of this file:  Any public Locations
 * (as returned by getLocation) would be an IndirectableLocation pointing
 * to an internal Location.  The latter contains a weak reference to the
 * public Location.  If/when the public Location is collected, then if
 * the internal location is unbound, it is removed from the Environment;
 * otherwise references to the Symbol name and property are replaced
 * by weak references; when they go away the internal location can be removed.
 */

/*
class Entry extends java.lang.ref.WeakReference
{
  // Normally same as (Symbol) get().
  Symbol symbol;

  // May be a weakreference.
  Object property;

  // If non-null, a weak reference to an exported location.
  EntryRef exported;

  Object value;
  Location base;

  public Entry (Symbol symbol, Object property,
		java.lang.ref.ReferenceQueue queue)
  {
    super(symbol, queue);
  }

  public Symbol getKeySymbol ()
  {
    if (symbol != null)
      return symbol;
    else
      return (Symbol) get();
  }

  public Object getKeyProperty ()
  {
    if (property instanceof WeakReference)
      {
	Object p = ((WeakReference) property).get();
	return p;
      }
    return property;
  }

  NamedLocation export ()
  {
    if (exported != null)
      {
	Location loc = (Location) exported.get();
	if (loc != null)
	  return loc;
	exported = null;
	// May also need to pull off queue.
      }
    PublicLocation ploc = new PublicLocation(this);
    exported = new EntryRef(ploc, queue, this);
    return ploc;
  }

  void unexport ()
  {
    symbol = null;
    exported = null;
    if (property != null && property != EnvironmentKey.FUNCTION)
      property = new EntryRef(property, queue);
  }
}

// A WeakReference to an PublicLocation or a property..
// We create a new sub-class do we can test for it in checkQueue.
class EntryRef extends WeakReference
{
  Entry entry;
  EntryRef(PublicLocation ploc, ReferenceQueue queue, Entry entry)
  {
    super(ploc, queue);
    this.entry = entry;
  }
}

class PublicLocation extends Location
{
  Entry entry;

  PublicLocation(Entry entry) { this.entry = entry; }
  
  public final Object get (Object defaultValue)
  {
    Entry e = this.entry;
    return e.base != null ? e.base.get(defaultValue)
      : e.value == Location.UNBOUND ? defaultValue : e.value;
  }
}

class SimpleEnvironment extends Environment
{
  Entry[] table;

  java.lang.ref.ReferenceQueue queue = new java.lang.ref.ReferenceQueue();

  void checkQueue ()
  {
  for (;;)
    {
      Reference ref = queue.poll();
      if (ref == null)
	return;
      if (ref instanceof EntryRef)
	{
	  Entry entry = ((EntryRef) ref).entry;
          if (entry.exported == ref)
	    entry.ref = null;
          else if (entry.exported == property)
	    remove(entry);
	}
      if (ref instanceof Entry)
	{
	  Entry entry = (Entry) ref;
	  remove(entry);
	}
    }
  }

  void remove (Entry entry)
  {
    unlink entry from this.table[index];
  }

  public Object get (Symbol key, Object property, Object defaultValue)
  {
    checkQueue();
    Entry e = lookupEntry(key, property);
    if (e == null)
      return defaultValue;
    return e.base != null ? e.base.get(defaultValue)
      : e.value == Location.UNBOUND ? defaultValue : e.value;
  }
}
*/
