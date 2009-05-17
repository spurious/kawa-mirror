package gnu.kawa.util;

public class WeakIdentityHashMap<K,V>
  extends AbstractHashTable<WeakHashNode<K,V>,K,V>
{
  /* #ifdef JAVA2 */
  java.lang.ref.ReferenceQueue<K> rqueue
    = new java.lang.ref.ReferenceQueue<K>();
  /* #endif */

  public WeakIdentityHashMap ()
  {
    super(64);
  }

  public WeakIdentityHashMap (int capacity)
  {
    super(capacity);
  }

  protected int getEntryHashCode (WeakHashNode<K,V> entry) { return entry.hash; }
  protected WeakHashNode<K,V> getEntryNext (WeakHashNode<K,V> entry)
  { return entry.next; }
  protected void setEntryNext (WeakHashNode<K,V> entry, WeakHashNode<K,V> next)
  { entry.next = next; }
  protected K getEntryKey (WeakHashNode<K,V> entry)
  { return entry.get(); }
  protected V getEntryValue (WeakHashNode<K,V> entry)
  { return entry.value; }
  protected void setEntryValue (WeakHashNode<K,V> entry, V value)
  { entry.value = value; }
  protected WeakHashNode<K,V>[] allocEntries(int n)
  { return (WeakHashNode<K,V>[]) new WeakHashNode[n]; }

  public int hash (K key)
  {
    return System.identityHashCode(key);
  }

  protected boolean matches (K key1, K key2)
  {
    return key1 == key2;
  }

  protected WeakHashNode<K,V> makeEntry (K key, int hash, V value)
  {
    WeakHashNode<K,V> node;
    /* #ifdef JAVA2 */
    node = new WeakHashNode<K,V>(key, rqueue, hash);
    /* #else */
    // node = new WeakHashNode<K,V>(key, hash);
    /* #endif */
    node.value = value;
    return node;
  }

  public V get (K key, V defaultValue)
  {
    cleanup();
    return super.get(key, defaultValue);
  }

  public V put (K key, int hash, V value)
  {
    cleanup();
    return super.put(key, hash, value);
  }

  public V remove (K key)
  {
    cleanup();
    return super.remove(key);
  }

  void cleanup () {
    AbstractWeakHashTable.cleanup(this, rqueue);
  }
}
