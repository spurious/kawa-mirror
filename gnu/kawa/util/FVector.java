package gnu.kawa.util;
import java.io.*;
import gnu.mapping.*;

public class FVector extends UniformVector implements Printable, Externalizable
{
  public String getTag() { return ""; }

  Object[] value;
  int length;

  public FVector ()
  {
  }

  public FVector (int num, Object o)
  {
    value = new Object[num];
    length = num;
    for (int i = 0;  i < num;  i++)
      value[i] = o;
  }

  public FVector (Object[] values)
  {
    length = values.length;
    value = values;
  }

  public final int length ()
  {
    return length;
  }

  public final Object get (int index)
  {
    if (index >= length)
      throw new ArrayIndexOutOfBoundsException();
    return value[index];
  }

  public boolean equals (Object obj)
  {
    if (obj == null || !(obj instanceof FVector))
      return false;
    FVector obj_vec = (FVector) obj;
    int n = length;
    if (obj_vec.value == null || obj_vec.length != n)
      return false;
    Object[] obj_value = obj_vec.value;
    for (int i = 0;  i < n;  i++)
      {
	if (! (value[i].equals (obj_value[i])))
	  return false;
      }
    return true;
  }

  public final void setElementAt (Object new_value, int index)
  {
    if (index >= length)
      throw new ArrayIndexOutOfBoundsException();
    value[index] = new_value;
  }

  public boolean add(Object o)
  {
    if (value == null)
      value = new Object[10];
    if (length == value.length)
      {
        Object[] buf = new Object[2 * length];
        System.arraycopy(value, 0, buf, 0, length);
        value = buf;
      }
    value[length] = o;
    length++;
    return true;
  }

  public final void setAll (Object new_value)
  {
     for (int index = length; --index >= 0; )
       value[index] = new_value;
  }

  public void print(java.io.PrintWriter ps)
  {
    int size = length;
    ps.print("#(");
    for (int t=0; t<size; t++)
      {
	if (t!=0)
	  ps.print(" ");
	SFormat.print (value[t], ps);
      }
    ps.print(")");
  }

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeObject).
   *   (It might seem simpler (and increase sharing) to just call
   *   writeObject(value), but that exposes the implementation.)
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeObject(value[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    Object[] value = new Object[len];
    length = len;
    for (int i = 0;  i < len;  i++)
      value[i] = in.readObject();
    this.value = value;
  }
}
