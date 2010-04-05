package gnu.kawa.util;
import java.util.*;

/** A hash table with weakly referenced keys and values.
 * Unlike java.util.WeakHashMap, this is useful when a
 * value object contain a strong reference to the corresponding keys.
 */

public abstract class AbstractWeakHashTable<K,V>
  extends AbstractHashTable<AbstractWeakHashTable.WEntry<K,V>,K,V>
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

  protected int getEntryHashCode (WEntry<K,V> entry) { return entry.hash; }
  protected WEntry<K,V> getEntryNext (WEntry<K,V> entry) { return entry.next; }
  protected void setEntryNext (WEntry<K,V> entry, WEntry<K,V> next) { entry.next = next; }
  protected WEntry<K,V>[] allocEntries(int n) { return (WEntry<K,V>[]) new WEntry[n]; }

  protected V getValueIfMatching (WEntry<K,V> node, Object key)
  {
    V val = node.getValue();
    if (val != null && matches(getKeyFromValue(val), key))
      return val;
    return null;
  }

  public V get (Object key, V defaultValue)
  {
    cleanup();
    return super.get(key, defaultValue);
  }

  public int hash (Object key)
  {
    return System.identityHashCode(key);
  }

  protected boolean valuesEqual (V oldValue, V newValue)
  {
    return oldValue == newValue;
  }

  protected WEntry<K,V> makeEntry (K key, int hash, V value)
  {
    return new WEntry<K,V>(value, this, hash);
  }

  public V put (K key, V value)
  {
    cleanup();
    return super.put(key, value);
  }

  protected void cleanup ()
  {
    /* #ifdef JAVA2 */
    cleanup(this, rqueue);
    /* #endif */
  }

  static <Entry extends Map.Entry<K,V>,K,V> void cleanup (AbstractHashTable<Entry,?,?> map,
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

  public static class WEntry<K,V>
    /* #ifdef JAVA2 */
    extends java.lang.ref.WeakReference<V>
    implements Map.Entry<K,V>
    /* #endif */
    {
      public WEntry next;
      public int hash;
      AbstractWeakHashTable<K,V> htable;

      public WEntry(V value, AbstractWeakHashTable<K,V> htable, int hash)
        {
          /* #ifdef JAVA2 */
          super(value, htable.rqueue);
          /* #else */
          // this.value = value;
          /* #endif */
          this.htable = htable;
          this.hash = hash;
        }
      /* #ifndef JAVA2 */
      // K key;
      // public K get() { return key; }
      /* #endif */

      public K getKey ()
      {
        V v = get();
        return v == null ? null : htable.getKeyFromValue(v);
      }

      public V getValue ()
      {
        return get();
      }

      public V setValue (V value)
      {
        throw new UnsupportedOperationException();
      }
    }
}
