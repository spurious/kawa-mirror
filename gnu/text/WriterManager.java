// Copyright (c) 2001  Per M.A. Bothner.
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
  public static WriterManager instance = new WriterManager();

  Writer[] ports;
  int[] freeList;
  int freeListHead = -1;

  public synchronized int register (Writer port)
  {
    if (freeListHead < 0)
      {
	int oldSize, newSize;
	if (ports == null)
	  {
	    oldSize = 0;
	    newSize = 20;
	  }
	else
	  {
	    oldSize = ports.length;
	    newSize = 2 * oldSize;
	  }
	int[] newFreeList = new int[newSize];
	Writer[] newPorts = new Writer[newSize];
	if (oldSize > 0)
	  {
	    System.arraycopy(ports, 0, newPorts, 0, oldSize);
	    System.arraycopy(freeList, 0, newFreeList, 0, oldSize);
	  }
	for (int i = oldSize;  i < newSize;  i++)
	  {
	    newFreeList[i] = freeListHead;
	    freeListHead = i;
	  }
	ports = newPorts;
	freeList = newFreeList;
      }
    int index = freeListHead;
    ports[index] = port;
    freeListHead = freeList[index];
    freeList[index] = -2;
    return index;
  }

  public synchronized void unregister (int index)
  {
    ports[index] = null;
    freeList[index] = freeListHead;
    freeListHead = index;
  }

  public void run()
  {
    if (ports == null)
      return;
    for (int i = ports.length;  --i >= 0; )
      {
	Writer port = ports[i];
	try
	  {
	    if (port != null)
	      port.close();
	  }
	catch (Exception ex)
	  {
	    // ignore
	  }
      }
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
