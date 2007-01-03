package gnu.mapping;
import gnu.lists.Consumer;
import java.io.PrintWriter;

/**
 * Similar to CharArrayWriter.
 */

public class CharArrayOutPort extends OutPort
{
  public CharArrayOutPort()
  {
    super(null, false, CharArrayInPort.stringPath);
  }

  public int length ()
  {
    return bout.bufferFillPointer;
  }

  public void setLength (int length)
  {
    bout.bufferFillPointer = length;
  }

  public void reset ()
  {
    bout.bufferFillPointer = 0;
  }

  /** Returns the written data as a freshly copied {@code char} array. */
  public char[] toCharArray()
  {
    int length = bout.bufferFillPointer;
    char[] result = new char[length];
    System.arraycopy(bout.buffer, 0, result, 0, length);
    return result;
  }

  /** Do nothing.
   * This allows access to the buffer after the port is closed.
   * Not clear whether this is a good or bad idea, but it matches
   * ByteArrayOutputStream, CharArrayWriter, and StringWriter.
   */
  public void close ()
  {
  }

  /** No point in registering this port with a WriterManager. */
  protected boolean closeOnExit ()
  {
    return false;
  }

  /** Returns the written data as a new {@code String}. */
  public String toString ()
  {
    return new String(bout.buffer, 0, bout.bufferFillPointer);
  }

  /** Returns a substring of the written data as a new {@code String}.
   * Equivalent to {@code toString().substring(beginIndex, endIndex)}
   * but more efficient.
   */
  public String toSubString (int beginIndex, int endIndex)
  {
    if (endIndex > bout.bufferFillPointer)
      throw new IndexOutOfBoundsException();
    return new String(bout.buffer, beginIndex, endIndex - beginIndex);
  }

  /** Returns a substring of the written data as a new {@code String}.
   * Equivalent to {@code toString().substring(beginIndex)}
   * but more efficient.
   */
  public String toSubString (int beginIndex)
  {
    return new String(bout.buffer, beginIndex,
                      bout.bufferFillPointer - beginIndex);
  }

  public void writeTo (Consumer out)
  {
    out.write(bout.buffer, 0, bout.bufferFillPointer);
  }

  public void writeTo (int start, int count, Consumer out)
  {
    out.write(bout.buffer, start, count);
  }
}

