// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector whose elements are Object references. */

public class FVector extends SimpleVector
implements Externalizable, Consumable
{
  public Object[] data;

  protected static Object[] empty = new Object[0];

  public FVector ()
  {
    data = empty;
  }

  public FVector (int num)
  {
    size = num;
    data = new Object[num];
  }

  public FVector (int num, Object o)
  {
    Object[] data = new Object[num];
    if (o != null)
      {
	for (int i = 0;  i < num;  i++)
	  data[i] = o;
      }
    this.data = data;
    this.size = num;
  }

  /** Reuses the argument without making a copy! */
  public FVector (Object[] data)
  {
    this.size = data.length;
    this.data = data;
  }

  /* BEGIN JAVA2 */
  public FVector(java.util.List seq)
  {
    this.data = new Object[seq.size()];
    addAll(seq);
  }
  /* END JAVA2 */
  /* BEGIN JAVA1 */
  // public FVector(Sequence seq)
  // {
    // this.data = new Object[seq.size()];
    // addAll(seq);
  // }
  /* END JAVA1 */

  /** Get the allocated length of the data buffer. */
  public int getBufferLength()
  {
    return data.length;
  }

  public void setBufferLength(int length)
  {
    int oldLength = data.length;
    if (oldLength != length)
      {
	Object[] tmp = new Object[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public void shift(int srcStart, int dstStart, int count)
  {
    System.arraycopy(data, srcStart, data, dstStart, count);
  }

  public final Object getBuffer(int index)
  {
    return data[index];
  }

  public final Object get (int index)
  {
    if (index >= size)
      throw new ArrayIndexOutOfBoundsException();
    return data[index];
  }

  public final Object setBuffer(int index, Object value)
  {
    Object old = data[index];
    data[index] = value;
    return old;
  }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = null;
  }

  public boolean equals (Object obj)
  {
    if (obj == null || !(obj instanceof FVector))
      return false;
    FVector obj_vec = (FVector) obj;
    int n = size;
    if (obj_vec.data == null || obj_vec.size != n)
      return false;
    Object[] obj_data = obj_vec.data;
    for (int i = 0;  i < n;  i++)
      {
	if (! (data[i].equals (obj_data[i])))
	  return false;
      }
    return true;
  }

  /*
  public final void setElementAt (Object new_value, int index)
  {
    if (index >= size)
      throw new ArrayIndexOutOfBoundsException();
    data[index] = new_value;
  }
  */

  // FIXME - bad name - setAll should take a Collection
  public final void setAll (Object new_value)
  {
     for (int i = size; --i >= 0; )
       data[i] = new_value;
  }

  public boolean consumeNext(int ipos, Consumer out)
  {
    int index = ipos >>> 1;
    if (index >= size)
      return false;
    out.writeObject(data[index]);
    return true;
  }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeObject(data[i]);
  }

  public void consume(Consumer out)
  {
    String typeName = "#vector"; 
    String type = typeName;
    out.beginGroup(typeName, type);
    int len = size;
    for (int i = 0;  i < len;  i++)
      out.writeObject(data[i]);
    out.endGroup(typeName);
  }

  /**
   * @serialData Write the number of elements (using writeInt), followed by
   *   the elements in order (written using writeObject).
   *   (It might seem simpler (and increase sharing) to just call
   *   writeObject(value), but that exposes the implementation.)
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int n = size;
    out.writeInt(n);
    for (int i = 0;  i < n;  i++)
      out.writeObject(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int n = in.readInt();
    Object[] data = new Object[n];
    for (int i = 0;  i < n;  i++)
      data[i] = in.readObject();
    size = n;
    this.data = data;
  }
}
