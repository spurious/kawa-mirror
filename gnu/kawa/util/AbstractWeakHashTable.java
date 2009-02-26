package gnu.kawa.util;

/** A hash table with weakly referenced keys and values.
 * Unlike java.util.WeakHashMap, this is useful when a
 * value object contain a strong reference to the corresponding keys.
 */

public abstract class AbstractWeakHashTable<K,V>
  extends AbstractHashTable<WeakHashNode<K,V>,K,V>
{
  /* #ifdef JAVA2 */
  java.lang.ref.ReferenceQueue<V> rqueue
    = new java.lang.ref.ReferenceQueue<V>();
  /* #endif */

  public AbstractWeakHashTable ()
  {
    super(64);
  }

  public AbstractWeakHashTable (int capacity)
  {
    super(capacity);
  }

  protected abstract K getKeyFromValue (V value);

  protected int getEntryHashCode (WeakHashNode<K,V> entry) { return entry.hash; }
  protected WeakHashNode<K,V> getEntryNext (WeakHashNode<K,V> entry) { return entry.next; }
  protected void setEntryNext (WeakHashNode<K,V> entry, WeakHashNode<K,V> next) { entry.next = next; }
  protected K getEntryKey (WeakHashNode<K,V> entry) { V t = getEntryValue(entry); return t==null ? null : getKeyFromValue(t); }
  protected V getEntryValue (WeakHashNode<K,V> entry) { return entry.get(); }
  protected void setEntryValue (WeakHashNode<K,V> entry, V value) { throw new UnsupportedOperationException(); }
  protected WeakHashNode<K,V>[] allocEntries(int n) { return (WeakHashNode<K,V>[]) new WeakHashNode[n]; }

  protected V getValueIfMatching (WeakHashNode<K,V> node, K key)
  {
    V val = getEntryValue(node);
    if (val != null && matches(getKeyFromValue(val), key))
      return val;
    return null;
  }

  public V get (K key, V defaultValue)
  {
    cleanup();
    int hash = hash(key);
    int index = hashToIndex(hash);
    for (WeakHashNode<K,V> node = table[index];
	 node != null;  node = node.next)
      {
        V val = getValueIfMatching(node, key);
        if (val != null)
          return val;
      }
    return defaultValue;
  }

  protected boolean valuesEqual (V oldValue, V newValue)
  {
    return oldValue == newValue;
  }

  public void put (K key, V value)
  {
    cleanup();
    int hash = hash(key);
    int index = hashToIndex(hash);
    WeakHashNode<K,V> first = table[index];
    WeakHashNode<K,V> node = first;
    WeakHashNode<K,V> prev = null;
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
            /* #ifdef JAVA2 */
            node = new WeakHashNode<K,V>(value, rqueue, hash, first);
            /* #else */
            // node = new WeakHashNode<K,V>(value, hash, first);
            /* #endif */
            table[index] = node;
	    return;
	  }
        V oldValue = getEntryValue(node);
        if (oldValue == value)
          return;
        WeakHashNode<K,V> next = node.next;
        if (oldValue != null && valuesEqual(oldValue, value))
          {
            if (prev == null)
              table[index] = next;
            else
              prev.next = next;
          }
        else
          prev = node;
	node = next;
      }
  }

  void cleanup ()
  {
    /* #ifdef JAVA2 */
    for (;;)
      {
        WeakHashNode<?,?> oldref = (WeakHashNode<?,?>) rqueue.poll();
        if (oldref == null)
          break;
        int index = hashToIndex(oldref.hash);
        WeakHashNode<K,V> prev = null;
        for (WeakHashNode<K,V> node = table[index];
             node != null;  )
          {
            WeakHashNode<K,V> next = node.next;
            if (node == oldref)
              {
                if (prev == null)
                  table[index] = next;
                else
                  prev.next = next;
                break;
              }
            prev = node;
            node = next;
          }
        num_bindings--;
      }
    /* #endif */
  }
}
