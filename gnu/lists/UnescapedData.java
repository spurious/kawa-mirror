// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Used for text that is supposed to be written out verbatim.
 * For example, the the output format is XML, can be used to write
 * a literal '<' as a plain "<", instead of being escaped as "&lt;".
 */

public class UnescapedData implements Externalizable, Consumable
{
  String data;

  public UnescapedData ()
  {
  }

  public UnescapedData (String data)
  {
    this.data = data;
  }

  public final String getData() { return data; }

  public final String toString() { return data; }

  public void consume(Consumer out)
  {
    out.writeChars(data);
  }

  public final boolean equals(Object other)
  {
    return other instanceof UnescapedData
      && data.equals(other.toString());
  }

  public final int hashCode() { return data == null ? 0 : data.hashCode(); }

  /**
   * @serialData Write 'data' (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(data);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    data = (String) in.readObject();
  }
}
