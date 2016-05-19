package gnu.kawa.io;

import gnu.lists.*;
import java.io.IOException;

/** An Inport for reading from a char array.
  * Essentially the same as an InPort wrapped around a CharArrayReader, but
  * more efficient because it uses the char array as the InPort's buffer. */

public class CharArrayInPort extends InPort
{
    static final Path stringPath = Path.valueOf(stringPathname);

    private AbstractCharVector string;
    /** Index in string corresponding to limit. */
    int limitIndex;
    int start, end; // currently only used if string!=null

  public static CharArrayInPort make(CharSequence seq)
  {
    int len = seq.length();
    if (seq instanceof FString)
        return ((FString) seq).openReader(0, len);
    else
      {
        char[] buf = new char[len];
        /* #ifdef use:java.lang.CharSequence */
        if (seq instanceof String)
          ((String) seq).getChars(0, len, buf, 0);
        else if (! (seq instanceof CharSeq))
          for (int i = len; --i >= 0; )
            buf[i] = seq.charAt(i);
        else
        /* #endif */
          ((CharSeq) seq).getChars(0, len, buf, 0);
        return new CharArrayInPort(buf, len);
      }
  }

  public CharArrayInPort (char[] buffer, int len)
  {
    super(NullReader.nullReader, stringPath);
    try
      {
	setBuffer(buffer);
      }
    catch (java.io.IOException ex)
      {
	throw new Error(ex.toString()); // Can't happen.
      }
    limit = len;
  }

  public CharArrayInPort (char[] buffer)
  {
    this(buffer, buffer.length);
  }

  public CharArrayInPort (String string)
  {
    this(string.toCharArray());
  }

    public CharArrayInPort(AbstractCharVector string, char[] buffer,
                           int start, int end) {
        this(buffer, 0);
        this.string = string;
        this.start = start;
        this.end = end;
        this.limitIndex = start;
    }

    @Override
    protected int fill(int len) throws java.io.IOException {
        if (string != null) {
            long result = string.getSegment(limitIndex);
            int where = (int) result;
            int size = (int) (result >> 32);
            if (size <= 0)
                return -1;
            limitIndex += size;
            if (limitIndex > end) {
                size -= limitIndex - end;
                limitIndex = end;
            }
            pos = where;
            limit = pos;
            return size;
        }
        return -1;

    }

    public void mark(int readAheadLimit) {
        synchronized (lock) {
            /* FIXME FUTURE
            if (string != null) {
                markPos = limitIndex - (limit - pos);
                this.readAheadLimit = readAheadLimit;
            } else
            */
                super.mark(readAheadLimit);
        }
    }
    public void reset() throws IOException {
        if (false/*FIXME*/ && string != null) {
            if (readAheadLimit < 0)
                throw new IOException ("mark invalid");
            // Minor potential optimization - requires being able
            // to determine if markPos is in the current "fragment".
            if (false) {
                pos = markPos + (limit - limitIndex);
            } else {
                limitIndex = markPos;
                pos = 0;
                limit = 0;
            }
            readAheadLimit = -1;
        } else {
            super.reset();
        }
    }
    /*
    public int read () throws java.io.IOException
  {
    if (pos >= limit)
      return -1;
    return super.read();
  }
    */
}
