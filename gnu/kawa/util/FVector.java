package gnu.kawa.util;
import java.io.*;
import gnu.mapping.*;

public class FVector extends UniformVector implements Printable, Externalizable
{
  public String getTag() { return ""; }

  Object[] value;

  public FVector ()
  {
  }

  public FVector (int num, Object o)
  {
    value = new Object[num];
    for (int i = 0;  i < num;  i++)
      value[i] = o;
  }

  public FVector (Object[] values)
  {
    value = values;
  }

  public final int length ()
  {
    return value.length;
  }

  public final Object get (int index)
  {
    return value[index];
  }

  public boolean equals (Object obj)
  {
    if (obj == null || !(obj instanceof FVector))
      return false;
    FVector obj_vec = (FVector) obj;
    int n = value.length;
    if (obj_vec.value == null || obj_vec.value.length != n)
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
    value[index] = new_value;
  }

  public final void setAll (Object new_value)
  {
     for (int index = value.length; --index >= 0; )
       value[index] = new_value;
  }

  public void print(java.io.PrintWriter ps)
  {
    int size = value.length;
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
    int len = value.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeObject(value[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    Object[] value = new Object[len];
    for (int i = 0;  i < len;  i++)
      value[i] = in.readObject();
    this.value = value;
  }
}
