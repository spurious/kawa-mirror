// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;
import gnu.mapping.*;

/** A generic hash table.
 * Supports deletions, and re-allocates the table when too big.
 * The equivalence relation can be customized. */

public class GeneralHashTable
{
  HashNode[] table;
  int log2Size;
  private int mask;
  int num_bindings;

  public GeneralHashTable ()
  {
    this(64);
  }

  public GeneralHashTable (int capacity)
  {
    log2Size = 4;
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
   * Override this and the {@link hash(Object)} method if you want
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

  public void put (Object key, Object value)
  {
    put(key, hash(key), value);
  }

  public void put (Object key, int hash, Object value)
  {
    int index = hash & mask;
    HashNode first = table[index];
    HashNode node = first;
    for (;;)
      {
	if (node == null)
	  {
            if (++num_bindings >= table.length)
              rehash();
            node = makeEntry(key, hash, value);
            node.next = first;
            table[index] = node;
	    return;
	  }
	else if (matches(key, hash, node))
	  {
	    node.setValue(value);
	    return;
	  }
	node = node.next;
      }
  }

  public void remove (Object key)
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
	      prev.next = node;
	    num_bindings--;
	    return;
	  }
	prev = node;
	node = next;
      }
  }

  void rehash ()
  {
    HashNode[] oldTable = table;
    int oldCapacity = oldTable.length;
    int newCapacity = 2 * oldCapacity;
    HashNode[] newTable = new HashNode[newCapacity];
    int newMask = newCapacity - 1;
    for (int i = oldCapacity;  --i >= 0;)
      {
	for (HashNode element = oldTable[i];  element != null; )
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
    log2Size++;
    mask = newMask;
  }
}
