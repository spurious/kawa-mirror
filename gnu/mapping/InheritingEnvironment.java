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

  public InheritingEnvironment (String name, Environment env)
  {
    super(name);
    addParent(env);
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

  public NamedLocation lookupDirect (Symbol name, Object property, int hash)
  {
    return super.lookup(name, property, hash);
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
	    IndirectableLocation xloc = newLocation(name, property);
	    xloc.value = IndirectableLocation.DIRECT_ON_SET;
	    xloc.base = loc;
	    addLocation(name, property, hash, xloc);
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
