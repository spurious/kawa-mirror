package gnu.mapping;
import java.io.*;

/**
 * Similar to CharArrayWriter.
 */

public class CharArrayOutPort extends OutPort
{
  public CharArrayOutPort(int bufsize)
  {
    super(null, bufsize, false);
    name = "<string>";
  }

  public CharArrayOutPort()
  {
    this(200);
  }

  public char[] toCharArray()
  {
    int length = bout.bufWritePos;
    char[] result = new char[length];
    System.arraycopy(bout.buffer, 0, result, 0, length);
    return result;
  }
}

