package gnu.text;
import java.io.*;

/** An InPort that reads from a queue.
  * The method append can be used to write chars to the end of the queue.
  * @author Per Bothner <bothner@cygnus.com>
  */

public class QueueReader extends Reader
{
  char[] buffer;
  int readAheadLimit;
  int mark;   // Mark position.
  int pos;    // Read position.
  int limit;  // Write position.
  boolean EOFseen;

  public QueueReader ()
  {
  }

  public boolean markSupported () { return true; }

  public synchronized void mark(int readAheadLimit)
  {
    this.readAheadLimit = readAheadLimit;
    mark = pos;
  }

  public synchronized void reset ()
  {
    if (readAheadLimit > 0)
      pos = mark;
  }

  void resize (int len)
  {
    int cur_size = limit - pos;
    if (readAheadLimit > 0 && pos - mark <= readAheadLimit)
      cur_size = limit - mark;
    else
      mark = pos;
    char[] new_buffer = (buffer.length < cur_size + len
			 ? new char[2 * cur_size + len]
			 : buffer);
    System.arraycopy(buffer, mark, new_buffer, 0, cur_size);
    buffer = new_buffer;
    pos -= mark;
    mark = 0;
    limit = cur_size;
  }

  public void append (String str)
  {
    append (str.toCharArray());
  }

  public void append(char[] chars)
  {
    append(chars, 0, chars.length);
  }

  public synchronized void append(char[] chars, int off, int len)
  {
    if (buffer == null)
      buffer = new char[100+len];
    else if (buffer.length < limit + len)
      resize(len);
    System.arraycopy(chars, off, buffer, limit, len);
    limit += len;
    notifyAll();
  }

  public synchronized void append(char ch)
  {
    if (buffer == null)
      buffer = new char[100];
    else if (buffer.length <= limit)
      resize(1);
    buffer[limit++] = ch;
    notifyAll();
  }

  /** For the writer to signal that there is no more data to append. */
  public synchronized void appendEOF ()
  {
    EOFseen = true;
  }

  public synchronized boolean ready ()
  {
    return pos < limit || EOFseen;
  }

  public synchronized int read ()
  {
    while (pos >= limit)
      {
	if (EOFseen)
	  return -1;
	try
	  {
	    wait();
	  }
	catch (java.lang.InterruptedException ex)
	  {
	  }
      }
    char ch = buffer[pos++];
    return ch;
  }

  public synchronized int read (char[] cbuf, int off, int len)
  {
    if (len == 0)
      return 0;
    while (pos >= limit)
      {
	if (EOFseen)
	  return -1;
	try
	  {
	    wait();
	  }
	catch (java.lang.InterruptedException ex)
	  {
	  }
      }
    int avail = limit - pos;
    if (len > avail)
      len = avail;
    System.arraycopy (buffer, pos, cbuf, off, len);
    pos += len;
    return len;
  }

  public synchronized void close ()
  {
    pos = 0;
    limit = 0;
    mark = 0;
    EOFseen = true;
    buffer = null;
  }
}
