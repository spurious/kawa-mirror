package gnu.mapping;

/**
 * Similar to CharArrayWriter.
 */

public class CharArrayOutPort extends OutPort
{
  public CharArrayOutPort()
  {
    super(null, false, "<string>");
  }

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

  public String toString()
  {
    return new String(bout.buffer, 0, bout.bufferFillPointer);
  }
}

