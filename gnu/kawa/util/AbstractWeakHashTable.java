package gnu.kawa.util;

/** A hash table with weakly referenced keys and values.
 * Unlike java.util.WeakHashMap, this is useful when a
 * value object contain a strong reference to the corresponding keys.
 */

public abstract class AbstractWeakHashTable<K,V>
  extends AbstractHashTable<WeakHashNode<V,V/*unused*/>,K,V>
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

  protected int getEntryHashCode (WeakHashNode<V,V> entry) { return entry.hash; }
  protected WeakHashNode<V,V> getEntryNext (WeakHashNode<V,V> entry) { return entry.next; }
  protected void setEntryNext (WeakHashNode<V,V> entry, WeakHashNode<V,V> next) { entry.next = next; }
  protected K getEntryKey (WeakHashNode<V,V> entry) { V t = getEntryValue(entry); return t==null ? null : getKeyFromValue(t); }
  protected V getEntryValue (WeakHashNode<V,V> entry) { return entry.get(); }
  protected void setEntryValue (WeakHashNode<V,V> entry, V value) { throw new UnsupportedOperationException(); }
  protected WeakHashNode<V,V>[] allocEntries(int n) { return (WeakHashNode<V,V>[]) new WeakHashNode[n]; }

  protected V getValueIfMatching (WeakHashNode<V,V> node, K key)
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
    for (WeakHashNode<V,V> node = table[index];
	 node != null;  node = node.next)
      {
        V val = getValueIfMatching(node, key);
        if (val != null)
          return val;
      }
    return defaultValue;
  }

  public int hash (K key)
  {
    return System.identityHashCode(key);
  }

  protected boolean valuesEqual (V oldValue, V newValue)
  {
    return oldValue == newValue;
  }

  protected WeakHashNode<V,V> makeEntry (K key, int hash, V value)
  {
    /* #ifdef JAVA2 */
    return new WeakHashNode<V,V>(value, rqueue, hash);
    /* #else */
    // return new WeakHashNode<V,V>(value, hash);
    /* #endif */
  }

  public V put (K key, V value)
  {
    cleanup();
    int hash = hash(key);
    int index = hashToIndex(hash);
    WeakHashNode<V,V> first = table[index];
    WeakHashNode<V,V> node = first;
    WeakHashNode<V,V> prev = null;
    V oldValue = null;
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
            node = makeEntry(null, hash, value);
            node.next = first;
            table[index] = node;
	    return oldValue;
	  }
        V curValue = getEntryValue(node);
        if (curValue == value)
          return curValue;
        WeakHashNode<V,V> next = node.next;
        if (curValue != null && valuesEqual(curValue, value))
          {
            if (prev == null)
              table[index] = next;
            else
              prev.next = next;
            oldValue = curValue;
          }
        else
          prev = node;
	node = next;
      }
  }

  protected void cleanup () {
    cleanup(this, rqueue);
  }

  static <Entry,K,V> void cleanup (AbstractHashTable<Entry,?,?> map,
                                   java.lang.ref.ReferenceQueue<?> rqueue)
  {
    /* #ifdef JAVA2 */
    for (;;)
      {
        Entry oldref = (Entry) rqueue.poll();
        if (oldref == null)
          break;
        int index = map.hashToIndex(map.getEntryHashCode(oldref));
        Entry prev = null;
        for (Entry node = map.table[index];
             node != null;  )
          {
            Entry next = map.getEntryNext(node);
            if (node == oldref)
              {
                if (prev == null)
                  map.table[index] = next;
                else
                  map.setEntryNext(prev, next);
                break;
              }
            prev = node;
            node = next;
          }
        map.num_bindings--;
      }
    /* #endif */
  }
}
