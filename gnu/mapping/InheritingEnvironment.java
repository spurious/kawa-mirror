// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class InheritingEnvironment extends SimpleEnvironment
{
  int numInherited;
  Environment[] inherited;
  Namespace[] namespaceMap;
  Object[] propertyMap;
  int baseTimestamp;

  public InheritingEnvironment (String name, SimpleEnvironment parent)
  {
    super(name);
    addParent(parent);
    int timestamp = ++parent.currentTimestamp;
    baseTimestamp = timestamp;
    currentTimestamp = timestamp;
  }

  public void addParent (Environment env)
  {
    if (numInherited == 0)
      inherited = new Environment[4];
    else if (numInherited <= inherited.length)
      {
	 Environment[] newInherited
	   = new Environment[2 * numInherited];
	 System.arraycopy(inherited, 0, newInherited, 0, numInherited);
	 inherited = newInherited;
      }
    inherited[numInherited] = env;
    numInherited++;
  }

  public NamedLocation lookupExtend (Symbol name, Object property, int hash)
  {
    return null;
  }

  public NamedLocation lookupInherited (Symbol name, Object property, int hash)
  { 
    for (int i = 0;  i < numInherited;  i++)
      {
	Symbol sym = name;
	Object prop = property;
	if (namespaceMap != null && namespaceMap.length > 2*i)
	  {
	    Object srcNamespace = namespaceMap[2*i];
	    Object dstNamespace = namespaceMap[2*i+1];
	    if (srcNamespace != null || dstNamespace != null)
	      {
		if (name.getNamespace() != dstNamespace)
		  continue;
		sym = Symbol.make(srcNamespace, name.getName());
	      }
	  }
	if (propertyMap != null && propertyMap.length > 2*i)
	  {
	    Object srcProperty = propertyMap[2*i];
	    Object dstProperty = propertyMap[2*i+1];
	    if (srcProperty != null || dstProperty != null)
	      {
		if (property != dstProperty)
		  continue;
		prop = srcProperty;
	      }
	  }
	NamedLocation loc = inherited[i].lookup(sym, prop, hash);
	if (loc != null && loc.isBound())
	  {
	    if (! (loc instanceof SharedLocation)
		|| ((SharedLocation) loc).timestamp < baseTimestamp)
	      return loc;
	  }
      }
    return null;
  }

  public NamedLocation lookup (Symbol name, Object property, int hash)
  {
    NamedLocation loc = super.lookup(name, property, hash);
    if (loc != null)
      return loc;
    loc = lookupInherited(name, property, hash);
    if (loc != null)
      return loc;
    return lookupExtend(name, property, hash);
  }

  public synchronized NamedLocation
  getLocation (Symbol name, Object property, boolean create)
  {
    int hash = name.hashCode() ^ System.identityHashCode(property);
    NamedLocation loc = lookupDirect(name, property, hash);
    if (loc != null)
      return loc;
    loc = lookupInherited(name, property, hash);

    if (loc != null)
      {
	if (create /* && loc.getEnvironment() != this*/)
	  {
	    NamedLocation xloc = addUnboundLocation(name, property, hash);
	    if ((flags & CAN_DEFINE) == 0 && loc.isBound())
	      redefineError(name, property, xloc);
	    xloc.base = loc;
	    if ((flags & DIRECT_INHERITED_ON_SET) != 0)
	      xloc.value = IndirectableLocation.DIRECT_ON_SET;
	    else
	      xloc.value = null;
	    if (xloc instanceof SharedLocation)
	      ((SharedLocation) xloc).timestamp = baseTimestamp;
	    return xloc;
	  }
	else
	  return loc;
      }
    loc = lookupExtend(name, property, hash);
    if (loc != null || ! create)
      return loc;
    return addUnboundLocation(name, property, hash);
  }

  public LocationEnumeration enumerateAllLocations()
  {
    LocationEnumeration it = new LocationEnumeration(table, 1 << log2Size);
    it.env = this;
    if (inherited != null && inherited.length > 0)
      {
	it.inherited = inherited[0].enumerateAllLocations();
	it.index = 0;
      }
    return it;
  }

  protected boolean hasMoreElements (LocationEnumeration it)
  {
    if (it.inherited != null)
      {
	for (;;)
	  {
	    NamedLocation loc = it.curLoc;
	    for (;;)
	      {
		it.inherited.curLoc = loc;
		if (! it.inherited.hasMoreElements())
		  {
		    it.curLoc = it.inherited.curLoc;
		    break;
		  }
		loc = it.inherited.curLoc;
		if (lookup(loc.name, loc.property) == loc)
		  {
		    it.curLoc = loc;
		    return true;
		  }
		loc = loc.next;
	      }
	    if (++it.index == numInherited)
	      break;
	    it.inherited = inherited[it.index].enumerateAllLocations();
	  }
	it.inherited = null;
	it.bindings = table;
	it.index = 1 << log2Size;
      }
    return super.hasMoreElements(it);
  }
}
