// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class BindingEnumeration implements java.util.Enumeration
{
  Binding[] bindings;
  int index;
  Environment parent;

  public BindingEnumeration(Binding[] bindings, int count, Environment parent)
  {
    this.bindings = bindings;
    index = count;
    this.parent = parent;
  }

  public BindingEnumeration(Binding[] bindings, int count)
  {
    this.bindings = bindings;
    index = count;
    this.parent = null;
  }

  public BindingEnumeration(Environment env)
  {
    this(env.table, 1 << env.log2Size, env.previous);
  }

  public boolean hasMoreElements()
  {
    for (;;)
      {
	while (index > 0)
	  {
	    Binding element = bindings[--index];
	    if (element != null && element != Binding.hashDELETED)
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

  public Binding nextBinding()
  {
    return bindings[--index];
  }
}
