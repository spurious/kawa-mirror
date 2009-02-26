// Copyright (c) 2005, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;

/** A generic hash table.
 * Supports deletions, and re-allocates the table when too big.
 * The equivalence relation can be customized. */

public class GeneralHashTable<K,V>
  extends AbstractHashTable<HashNode<K,V>,K,V>
  // FUTURE:  implements java.util.Map<K,V>
{
  public GeneralHashTable ()
  {
    super(64);
  }

  public GeneralHashTable (int capacity)
  {
    super(capacity);
  }

  protected int getEntryHashCode (HashNode<K,V> entry) { return entry.hash; }
  protected HashNode<K,V> getEntryNext (HashNode<K,V> entry) { return entry.next; }
  protected void setEntryNext (HashNode<K,V> entry, HashNode<K,V> next) { entry.next = next; }
  protected K getEntryKey (HashNode<K,V> entry) { return entry.key; }
  protected V getEntryValue (HashNode<K,V> entry) { return entry.value; }
  protected void setEntryValue (HashNode<K,V> entry, V value) { entry.value = value; }
  protected HashNode<K,V>[] allocEntries(int n) { return (HashNode<K,V>[]) new HashNode[n]; }

  /** Allocate a new node in the hash table. */
  protected HashNode<K,V> makeEntry (K key, int hash, V value)
  {
    HashNode<K,V> node = new HashNode<K,V>();
    node.key = key;
    node.hash = hash;
    node.value = value;
    return node;
  }

  public V put (K key, V value)
  {
    return put(key, hash(key), value);
  }

  public V put (K key, int hash, V value)
  {
    int index = hashToIndex(hash);
    HashNode<K,V> first = table[index];
    HashNode<K,V> node = first;
    for (;;)
      {
	if (node == null)
	  {
            if (++num_bindings >= table.length)
              {
                rehash();
                index = hashToIndex(hash);
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

  public V remove (K key)
  {
    int hash = hash(key);
    int index = hashToIndex(hash);
    HashNode<K,V> prev = null;
    HashNode<K,V> node = table[index];
    while (node != null)
      {
	HashNode<K,V> next = node.next;
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
}
