package gnu.mapping;
import java.io.*;

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

  public String toString()
  {
    return new String(bout.buffer, 0, bout.bufferFillPointer);
  }
}

