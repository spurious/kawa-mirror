// Copyright (c) 2001, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import java.io.*;

/** Manages a collection of Writers, handling automatic closing.
 * This class is useful for making sure that a Writer is closed (and its
 * buffers flushed) when a VM exits.
 * A WriterManager can be usefully passed to the JDK 1.3 method
 * addShutdownHook in Runtime.
 */

public class WriterManager implements Runnable
{
  public static final WriterManager instance = new WriterManager();

  WriterRef first;

  /** Register a Writer.
   * @return an object that can be passed to {@link #unregister}.
   */
  public synchronized WriterRef register (Writer port)
  {
    WriterRef ref = new WriterRef(port);
    WriterRef first = this.first; // Copy field to local variable.
    if (first != null)
      {
        ref.next = first.next;
        first.prev = ref;
      }
    this.first = ref;
    return ref;
  }

  /** Unregister a Writer.
   * @param key the object returned by the correspodning {@link #register}.
   */
  public synchronized void unregister (Object key)
  {
    if (key == null)
      return;
    WriterRef ref = (WriterRef) key;
    WriterRef next = ref.next;
    WriterRef prev = ref.prev;
    if (next != null)
      next.prev = prev;
    if (prev != null)
      prev.next = next;
    if (ref == first)
      first = next;
  }

  public synchronized void run()
  {
    for (WriterRef ref = first;  ref != null;  ref = ref.next)
      {
        Object port = ref.get();
        if (port != null)
          {
            try
              {
                ((Writer) port).close();
              }
            catch (Exception ex)
              {
                // ignore
              }
          }
      }
    first = null;
  }

  /** Try to register this as a shutdown hook.
   * @return true on success; false if failure (e.g. if not JDK1.3-compatible).
   */
  public boolean registerShutdownHook()
  {
    try
      {
	Runtime runtime = Runtime.getRuntime();
	Class rclass = runtime.getClass();
	Class[] params = { Thread.class };
	java.lang.reflect.Method method
	  = rclass.getDeclaredMethod("addShutdownHook", params);
	Object[] args = { new Thread(this) };
	method.invoke(runtime, args);
	return true;
      }
    catch (Throwable ex)
      {
	return false;
      }
  }
}

class WriterRef
/* #ifdef JAVA2 */
extends java.lang.ref.WeakReference
/* #endif */
{
  WriterRef next;
  WriterRef prev;

  public WriterRef (Writer wr)
  {
    super(wr);
  }
}
