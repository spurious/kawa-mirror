// Copyright (c) 2000, 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class SymbolEnumeration implements java.util.Enumeration
{
  Symbol[] bindings;
  int index;
  Environment parent;

  public SymbolEnumeration(Symbol[] bindings, int count, Environment parent)
  {
    this.bindings = bindings;
    index = count;
    this.parent = parent;
  }

  public SymbolEnumeration(Symbol[] bindings, int count)
  {
    this.bindings = bindings;
    index = count;
    this.parent = null;
  }

  public SymbolEnumeration(Environment env)
  {
    this(env.table, 1 << env.log2Size, env.previous);
  }

  public boolean hasMoreElements()
  {
    for (;;)
      {
	while (index > 0)
	  {
	    Symbol element = bindings[--index];
	    if (element != null && element != Symbol.hashDELETED)
	      {
		index++;
		return true;
	      }
	  }
	if (parent == null)
	  return false;
	bindings = parent.table;
	index = 1 << parent.log2Size;
	parent = parent.previous;
      }
  }

  public Object nextElement()
  {
    if (hasMoreElements())
      return bindings[--index];
    throw new java.util.NoSuchElementException();
  }

  public Symbol nextSymbol()
  {
    return bindings[--index];
  }
}
