// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;
import java.util.*;

/* A constant vector of arbitrary objects.
 * This class is probably no longer needed,
 * single any SimpleVector can now be made constant.  FIXME.
 */

public class ConstVector<E> extends FVector<E>
{
    public ConstVector() {
        info |= READ_ONLY_FLAG;
    }

    public ConstVector(Object[] data) {
        super(data);
        info |= READ_ONLY_FLAG;
    }

  public ConstVector(java.util.List seq)
  {
    this(new Object[seq.size()]);
    int index = 0;
    for (Iterator<?> it = seq.iterator();  it.hasNext(); )
      {
        data[index++] = it.next();
      }
  }

  public static ConstVector make(Object... data)
  {
    return new ConstVector(data);
  }

  protected void checkCanWrite ()
  {
    throw new UnsupportedOperationException();
  }

  public void setDataBackDoor(Object[] data)
  {
    this.data = data;
  }

  /**
   * @serialData Write the number of elements (using writeInt), followed by
   *   the elements in order (written using writeObject).
   *   (It might seem simpler (and increase sharing) to just call
   *   writeObject(value), but that exposes the implementation.)
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int n = size();
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
    this.data = data;
  }
}
