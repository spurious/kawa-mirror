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
  public static final int DEFAULT_INITIAL_SIZE = 64;

  public GeneralHashTable ()
  {
    super(DEFAULT_INITIAL_SIZE);
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

  /** This override helps Kawa type-inference - for example in srfi69.scm. */
  public HashNode<K,V> getNode (K key)
  {
    return super.getNode(key);
  }

}
