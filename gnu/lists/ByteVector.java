package gnu.lists;
import java.io.*;

public abstract class ByteVector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  byte[] data;
  protected static byte[] empty = new byte[0];

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
	byte[] tmp = new byte[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final byte byteAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return data[index];
  }

  public final byte byteAtBuffer(int index)
  {
    return data[index];
  }

  public boolean consumeNext (int ipos, Consumer out)
  {
    int index = ipos >>> 1;
    if (index >= size)
      return false;
    out.writeInt(intAtBuffer(index));
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
      out.writeInt(intAtBuffer(i));
  }

  public final void setByteAt(int index, byte value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setByteAtBuffer(int index, byte value)
  {
    data[index] = value;
  }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = 0;
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeByte).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeByte(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    byte[] data = new byte[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readByte();
    this.data = data;
    this.size = size;
  }
}
