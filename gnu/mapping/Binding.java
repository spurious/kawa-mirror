// Copyright (c) 1996-2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Binding is a Location in an Environment object. */

public class Binding extends Location
    // implements java.util.Map.Entry
{
  /** The current value of the binding. */
  Object value;

  Constraint constraint;

  public final Object get ()
  {
    return constraint.get(this);
  }

  public final Procedure getProcedure ()
  {
    return constraint.getProcedure(this);
  }

  public final void set (Object value)
  {
    constraint.set(this, value);
  }

  public final Constraint getConstraint()
  {
    return constraint;
  }

  public final void setConstraint (Constraint constraint)
  {
    this.constraint = constraint;
  }

  public boolean isBound ()
  {
    return constraint.isBound(this);
  }

  public Binding (String name)
  {
    setName(name); 
  }

  static final TrivialConstraint trivialConstraint
    = new TrivialConstraint(null);

  // The compiler emits calls to this method.
  public static Binding make (Object init, String name)
  {
    Binding binding = new Binding(name);
    binding.value = init;
    binding.constraint = trivialConstraint;
    return binding;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<binding ");
    String name = getName();
    if (name != null)
      ps.print(name);
    if (isBound())
      {
	ps.print(" -> ");
	SFormat.print(get(), ps);
      }
    else
      ps.print("(unbound)");
    ps.print ('>');
  }

  // Methods that implement java.util.Map.Entry:

  public final Object getKey ()
  {
    return getName();
  }

  public final Object getValue ()
  {
    return constraint.get(this);
  }

  public final Object setValue (Object value)
  {
    Object old = constraint.get(this);
    constraint.set(this, value);
    return old;
  }

  public boolean equals (Object o)
  {
    if (! (o instanceof Binding))
      return false;
    Binding e2 = (Binding) o;
    String e1Key = getName();
    String e2Key = e2.getName();
    // This is quite following the Collections spec, but we assume keys
    // are interned, or if they are not, that they are seen as unequal.
    if (e1Key != e2Key)
      return false;
    Object e1Value = constraint.get(this);
    Object e2Value = e2.constraint.get(e2);
    return e1Value == null ? e2Value == null : e1Value.equals(e2Value);
  }

  public int hashCode ()
  {
    // Note:  The hashCode should not depend on the value.
    return System.identityHashCode(this); // ^ System.identityHashCode(env);
  }

  /** Used to mark deleted elements in a hash table. */
  public final static Binding hashDELETED
  = new Binding(new String("<Deleted>"));

  /** Search a hash table using double hashing and open addressing.
   * @param table the hash table
   * @param log2Size log2 of the (used) size of table
   * @param mask must equal ((1 << log2Size) - 1)
   * @param key the search key
   * @param hash the hash of the search key
   * @return the index of the element in table containing the match
   * (such that table[index].getName()==key);
   * if there is no such element, returns an index
   * such that (table[index]==null || tabel[index]==DELETED). */
  public static int hashSearch (Binding[] table, int log2Size, int mask,
				String key, int hash)
  {
    int index = hash & mask;
    Binding element = table[index];
    if (element == null || element.getName() == key)
      return index;
    int avail = -1;
    int step = (((hash >> log2Size) ^ index) << 1) + 1;
    for (;;)
      {
	if (element == hashDELETED && avail < 0)
	  avail = index;
	index = (index + step) & mask;
	element = table[index];
	if (element == null)
	  return avail < 0 ? index : avail;
	if (element.getName() == key)
	  return index;
      }
  }

  public static int hashSearch (Binding[] table, int log2Size, String key)
  {
    return hashSearch(table, log2Size, (1 << log2Size) - 1,
		      key, System.identityHashCode(key));
  }

  /** Find an entry in a hash table.
   * @param table the hash table
   * @param log2Size log2 of the (used) size of table
   * @param key the search key
   * @return null if the was no matching element in the hash table;
   * otherwise the matching element. */
  public static Binding hashGet (Binding[] table, int log2Size, String key)
  {
    int index = hashSearch(table, log2Size, (1 << log2Size) - 1,
			   key, System.identityHashCode(key));
    Binding element = table[index];
    if (element == null || element == hashDELETED)
      return null;
    return element;
  }

  /** Set an entry in a hash table.
   * @param table the hash table
   * @param log2Size log2 of the (used) size of table
   * @param value the new entry
   * @return null if the was no matching element in the hash table;
   * otherwise the old match. */
  public static Binding hashSet (Binding[] table, int log2Size, Binding value)
  {
    String key = value.getName();
    int index = hashSearch(table, log2Size, (1 << log2Size) - 1,
			   key, System.identityHashCode(key));
    Binding element = table[index];
    table[index] = value;
    return element == hashDELETED ? null : element;
  }

  /** Delete an entry from a hash table.
   * @param table the hash table
   * @param log2Size log2 of the (used) size of table
   * @param key the search key
   * @return null if the was no matching element in the hash table;
   * otherwise the old element. */
  public static Binding hashDelete (Binding[] table, int log2Size, String key)
  {
    int index = hashSearch(table, log2Size, (1 << log2Size) - 1,
			   key, System.identityHashCode(key));
    Binding element = table[index];
    table[index] = hashDELETED;
    return element == hashDELETED ? null : element;
  }

  public static int hashInsertAll (Binding[] tableDst, int log2SizeDst,
				   Binding[] tableSrc, int log2SizeSrc)
  {
    int countInserted = 0;
    int sizeSrc = 1 << log2SizeSrc;
    int sizeDst = 1 << log2SizeDst;
    int maskDst = (1 << log2SizeDst) - 1;
    for (int i = sizeSrc;  --i >= 0;)
      {
	Binding element = tableSrc[i];
	if (element != null && element != hashDELETED)
	  {
	    String key = element.getName();
	    int index = hashSearch(tableDst, log2SizeDst, maskDst,
				   key, System.identityHashCode(key));
	    Binding oldElement = tableDst[index];
	    if (oldElement != null && oldElement != hashDELETED)
	      countInserted++;
	    tableDst[index] = element;
	  }
      }
    return countInserted;
  }

  public final Object getFunctionValue()
  {
    Binding binding = this;
    while (binding.constraint instanceof AliasConstraint)
      binding = (Binding) binding.value;
    if (binding instanceof Binding2)
      return ((Binding2) binding).functionValue;
    else if (binding.isBound())
      return binding.get();
    else
      return null;
  }
}
