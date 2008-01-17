// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;

/** A generic hash table.
 * Supports deletions, and re-allocates the table when too big.
 * The equivalence relation can be customized. */

public class GeneralHashTable
// FUTURE: implements java.util.Map
{
  protected HashNode[] table;
  protected int mask;
  protected int num_bindings;

  public GeneralHashTable ()
  {
    this(64);
  }

  public GeneralHashTable (int capacity)
  {
    int log2Size = 4;
    while (capacity > (1 << log2Size))
      log2Size++;
    capacity = 1 << log2Size;
    table = new HashNode[capacity];
    mask = capacity - 1;
  }

  /** Allocate a new node in the hash table. */
  protected HashNode makeEntry (Object key, int hash, Object value)
  {
    HashNode node = new HashNode();
    node.key = key;
    node.hash = hash;
    node.value = value;
    return node;
  }

  /** Calculate hash code of a key.
   * You may need to override this if you override the <code>matches</code> method.
   */
  public int hash (Object key)
  {
    // FIXME
    return key == null ? 0 : key.hashCode();
  }

  public int hash (HashNode node)
  {
    //return hash(node.getKey());
    return node.hash;
  }

  public boolean matches (Object key, int hash, HashNode node)
  {
    return node.hash == hash && matches(node.getKey(), key);
  }

  /** Compare two keys for equivalence.
   * Override this and the {@link #hash(Object)} method if you want
   * a different equivalence relation.
   */
  public boolean matches (Object value1, Object value2)
  {
    // FIXME
    return value1 == value2 || (value1 != null && value1.equals(value2));
  }

  public Object get (Object key, Object defaultValue)
  {
    int hash = hash(key);
    int index = hash & this.mask;
    for (HashNode node = table[index];
	 node != null;  node = node.next)
      {
	if (matches(key, hash, node))
	  return node.getValue();
      }
    return defaultValue;
  }

  public HashNode getNode (Object key)
  {
    int hash = hash(key);
    int index = hash & this.mask;
    for (HashNode node = table[index];
	 node != null;  node = node.next)
      {
	if (matches(key, hash, node))
          return node;
      }
    return null;
  }

  public Object put (Object key, Object value)
  {
    return put(key, hash(key), value);
  }

  public Object put (Object key, int hash, Object value)
  {
    int index = hash & mask;
    HashNode first = table[index];
    HashNode node = first;
    for (;;)
      {
	if (node == null)
	  {
            if (++num_bindings >= table.length)
              {
                rehash();
                index = hash & mask;
                first = table[index];
              }
            node = makeEntry(key, hash, value);
            node.next = first;
            table[index] = node;
	    return null;
	  }
	else if (matches(key, hash, node))
	  {
	    return node.setValue(value);
	  }
	node = node.next;
      }
  }

  public Object remove (Object key)
  {
    int hash = hash(key);
    int index = hash & this.mask;
    HashNode prev = null;
    HashNode node = table[index];
    while (node != null)
      {
	HashNode next = node.next;
	if (matches(key, hash, node))
	  {
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.next = next;
	    num_bindings--;
	    return node.getValue();
	  }
	prev = node;
	node = next;
      }
    return null;
  }

  protected void rehash ()
  {
    HashNode[] oldTable = table;
    int oldCapacity = oldTable.length;
    int newCapacity = 2 * oldCapacity;
    HashNode[] newTable = new HashNode[newCapacity];
    int newMask = newCapacity - 1;
    for (int i = oldCapacity;  --i >= 0;)
      {
        HashNode chain = oldTable[i];
        if (chain != null && chain.next != null)
          {
            // Reverse the old chain in place, so that after re-hashing the
            // new chain has the same order.. This is useful for some
            // subclasses (specifically gnu.expr.NameLookup), and it is
            // cheap to so here where extra cache misses are unlikely.
            HashNode prev = null;
            do
              {
                HashNode node = chain;
                chain = node.next;
                node.next = prev;
                prev = node;
              }
            while (chain != null);
            chain = prev;
          }

	for (HashNode element = chain;  element != null; )
	  {
	    HashNode next = element.next;
	    int hash = hash(element);
	    int j = hash & newMask;
	    HashNode head = newTable[j];
	    element.next = head;
	    newTable[j] = element;
	    element = next;
	  }
      }
    table = newTable;
    mask = newMask;
  }

  public void clear ()
  {
    HashNode[] t = this.table;
    for (int i = t.length;  --i >= 0; )
      t[i] = null;
    num_bindings = 0;
  }

  public int size ()
  {
    return num_bindings;
  }

  protected static HashNode next (HashNode node)
  {
    return node.next;
  }
}
