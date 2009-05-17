// Copyright (c) 2005, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;

/** An abstract hash map from K to V.
 * The entries are represented by an Entry type parameter.
 */

public abstract class AbstractHashTable<Entry, K, V>
{
  protected Entry[] table;
  protected int mask;
  protected int num_bindings;

  /** Extract hash-code from Entry. */
  protected abstract int getEntryHashCode (Entry entry);
  /** Extract next Entry in same hash-bucket. */
  protected abstract Entry getEntryNext (Entry entry);
  /** Set next Entry in same hash-bucket. */
  protected abstract void setEntryNext (Entry entry, Entry next);
  /** Extract key from Entry. */
  protected abstract K getEntryKey (Entry entry);
  /** Extract mapped value from Entry. */
  protected abstract V getEntryValue (Entry entry);
  /** Set mapped value in Entry. */
  protected abstract void setEntryValue (Entry entry, V value);
  /** Allocate Entry[n]. */
  protected abstract Entry[] allocEntries(int n);

  public AbstractHashTable (int capacity)
  {
    int log2Size = 4;
    while (capacity > (1 << log2Size))
      log2Size++;
    capacity = 1 << log2Size;
    table = allocEntries(capacity);
    mask = capacity - 1;
  }

  /** Allocate a new node in the hash table. */
  protected abstract Entry makeEntry (K key, int hash, V value);

  /** Calculate hash code of a key.
   */
  public int hash (K key)
  {
    return key == null ? 0 : key.hashCode();
  }

  /** Map hash-code to bucket index in table. */
  protected int hashToIndex (int hash)
  {
    // Very basic re-arranging of the bits in case of poor hash.
    hash ^= hash >>> 15;
    return hash & mask;
  }

  /** True if an Entry matches a key. */
  protected boolean matches (K key, int hash, Entry node)
  {
    return getEntryHashCode(node) == hash && matches(getEntryKey(node), key);
  }

  /** Compare two keys for equivalence.
   * Override this and the {@link #hash(Object)} method if you want
   * a different equivalence relation.
   */
  protected boolean matches (K key1, K key2)
  {
    return key1 == key2 || (key1 != null && key1.equals(key2));
  }

  /** Find value for given key.  Return null if not found. */
  public V get (K key)
  {
    return get(key, null);
  }

  /** Find Entry for given key.  Return null if not found. */
  public Entry getNode (K key)
  {
    int hash = hash(key);
    int index = hashToIndex(hash);
    for (Entry node = table[index];
	 node != null;  node = getEntryNext(node))
      {
	if (matches(key, hash, node))
	  return node;
      }
    return null;
  }

  /** Find value for given key.  Return defaultValue if not found. */
  public V get (K key, V defaultValue)
  {
    Entry node = getNode(key);
    return node == null ? defaultValue : getEntryValue(node);
  }

  protected void rehash ()
  {
    Entry[] oldTable = table;
    int oldCapacity = oldTable.length;
    int newCapacity = 2 * oldCapacity;
    Entry[] newTable = allocEntries(newCapacity);
    int newMask = newCapacity - 1;
    table = newTable;
    mask = newMask;
    for (int i = oldCapacity;  --i >= 0;)
      {
        Entry chain = oldTable[i];
        if (chain != null && getEntryNext(chain) != null)
          {
            // Reverse the old chain in place, so that after re-hashing the
            // new chain has the same order.. This is useful for some
            // subclasses (specifically gnu.expr.NameLookup), and it is
            // cheap to do here where extra cache misses are unlikely.
            Entry prev = null;
            do
              {
                Entry node = chain;
                chain = getEntryNext(node);
                setEntryNext(node, prev);
                prev = node;
              }
            while (chain != null);
            chain = prev;
          }

	for (Entry element = chain;  element != null; )
	  {
	    Entry next = getEntryNext(element);
	    int hash = getEntryHashCode(element);
	    int j = hashToIndex(hash);
	    Entry head = newTable[j];
	    setEntryNext(element, head);
	    newTable[j] = element;
	    element = next;
	  }
      }
  }

  public V put (K key, V value)
  {
    return put(key, hash(key), value);
  }

  public V put (K key, int hash, V value)
  {
    int index = hashToIndex(hash);
    Entry first = table[index];
    Entry node = first;
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
            setEntryNext(node, first);
            table[index] = node;
	    return null;
	  }
	else if (matches(key, hash, node))
	  {
            V oldValue = getEntryValue(node);
            setEntryValue(node, value);
	    return oldValue;
	  }
	node = getEntryNext(node);
      }
  }

  public V remove (K key)
  {
    int hash = hash(key);
    int index = hashToIndex(hash);
    Entry prev = null;
    Entry node = table[index];
    while (node != null)
      {
	Entry next = getEntryNext(node);
	if (matches(key, hash, node))
	  {
	    if (prev == null)
	      table[index] = next;
	    else
	      setEntryNext(prev, next);
	    num_bindings--;
	    return getEntryValue(node);
	  }
	prev = node;
	node = next;
      }
    return null;
  }

  public void clear ()
  {
    Entry[] t = this.table;
    for (int i = t.length;  --i >= 0; )
      setEntryNext(t[i], null);
    num_bindings = 0;
  }

  public int size ()
  {
    return num_bindings;
  }
}
