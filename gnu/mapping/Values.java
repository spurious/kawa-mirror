package gnu.mapping;
import java.io.*;

/** Encapsulate multiple values in a single object.
 * In Scheme and Lisp mainly used to return multiple values from a function.
 */

public class Values implements Printable, Externalizable
{
  public static final Object[] noArgs = new Object[0];
  private Object[] vals;

  public static final Values empty = new Values(noArgs);

  public Values ()
  {
    vals = noArgs;
  }

  /** Constructor.
   * @param values the values to encapulate
   */
  public Values (Object[] values)
  {
    vals = values;
  }

  /** Get the values encapsulated. */
  public Object[] getValues ()
  {
    return vals;
  }

  public static Object values$V(Object[] vals)
  {
    return make(vals);
  }

  public static Object make (Object[] vals)
  {
    if (vals.length == 1)
      return vals[0];
    else if (vals.length == 0)
      return empty;
    else
      return new Values(vals);    
  }

  /** Apply a Procedure with these values as the arguments. */
  public Object call_with (Procedure proc)
  {
    return proc.applyN (vals);
  }

  public void print(java.io.PrintWriter ps)
  {
    if (this == empty)
      {
	ps.print("#!void");
	return;
      }
    int size = vals.length;
    ps.print("#<values");
    for (int i = 0; i < size; i++)
      {
	ps.print (" ");
	SFormat.print (vals[i], ps);
      }
    ps.print (">");
  }

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the values in order (written using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = vals.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeObject(vals[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    Object[] data = len == 0 ? noArgs : new Object[len];
    for (int i = 0;  i < len;  i++)
      data[i] = in.readObject();
    this.vals = data;
  }

  public Object readResolve() throws ObjectStreamException
  {
    return vals.length == 0 ? empty : this;
  }

}
