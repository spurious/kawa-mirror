package gnu.kawa.util;

public class WeakHashNode<K,V>
/* #ifdef JAVA2 */
extends java.lang.ref.WeakReference<V>
/* #endif */
{
  public WeakHashNode<K,V> next;
  int hash;

  public WeakHashNode(V value,
                      /* #ifdef JAVA2 */
                      java.lang.ref.ReferenceQueue<V> q,
                      /* #endif */
                      int hash, WeakHashNode<K,V> next)
  {
    /* #ifdef JAVA2 */
    super(value, q);
    /* #else */
    // this.value = value;
    /* #endif */
    this.hash = hash;
    this.next = next;
  }
  /* #ifndef JAVA2 */
  // V value;
  // public V get() { return value; }
  /* #endif */
}
