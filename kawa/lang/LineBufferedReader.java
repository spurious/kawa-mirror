package kawa.lang;  // Should move to gnu.text!
import java.io.*;

/** A LineNumberReader with some extra features:
  *
  * You can seek backwards to the start of the line preceding the
  * current position (or the mark, if that has been set).
  * You can use seek with a negative offset, or unread.
  * You can also use peek to look at the next character without moving.
  *
  * The method getColumnNumber gives you the current column.
  *
  * Provides a method that is called at the start of a line.
  * This is especially useful for interactive streams (e.g. prompting).
  *
  * It would be nice if we could inherit from LineNumberReader.
  * That may be possible in theory, but it is difficult and
  * expensive (because we don't get access to BufferedReader's buffer).
  */

public class LineBufferedReader extends FilterReader
{
  final static int BUFFER_SIZE = 1024;
  protected char[] buffer;

  int pos;

  int limit;

  /** True if CR and CRLF should be converted to LF. */
  boolean convertCR = true;

  public void setConvertCR(boolean convertCR) { this.convertCR = convertCR; }

  // The current line number (at start of buffer).
  int lineNumber;

  // The position that marks the start of the current line, or -1 if unknown.
  int lineStartPos;

  private boolean atEol = true;

  private int readAheadLimit = 0;

  private int markPos;

  private int highestLineNumber = -1;

  public LineBufferedReader (InputStream in)
  {
    super (new InputStreamReader(in));
  }

  public LineBufferedReader (Reader in)
  {
    super (in);
  }

  /** A hook to allow sub-classes to perform some action at start of line.
    * Called just before the first character of the new line is read.
    * @param revisited true if we have read here before (i.e.
    *   we did a reset of unread() to get here).
    */
  public void lineStart (boolean revisited) throws java.io.IOException
  {
  }

  private void lineStart () throws java.io.IOException
  {
    int lineno = getLineNumber();
    boolean revisited = lineno <= highestLineNumber;
    if (! revisited)
      highestLineNumber = lineno;
    lineStart(revisited);
  }

  public int readHook (char[] buffer, int off, int len) throws java.io.IOException
  {
    return len;
  }

  public int read () throws java.io.IOException
  {
    if (getColumnNumber() == 0)
      lineStart();
    if (pos >= limit)
      {
	// Converting CRLF to LF is made more complicated because
	// we want to be able to return ready()
	boolean seenCR = false;
	if (buffer == null)
	  buffer = new char[BUFFER_SIZE];
	else if (limit == buffer.length)
	  {
	    if (pos - markPos > readAheadLimit)
	      readAheadLimit = 0;
	    // Calculate how much to save from the existing buffer.  This
	    // should be the last line starting before both markPos and pos.
	    int saveStart = pos;
	    if (readAheadLimit > 0 && pos > markPos)
	      saveStart = markPos;
	    if (saveStart >= lineStartPos)
	      saveStart = lineStartPos;
	    else
	      {
		while (saveStart > 0)
		  {
		    char ch = buffer[saveStart-1];
		    if (ch == '\n' || ch == '\r')
		      break;
		    saveStart--;
		  }
	      }

	    lineNumber += countLines (buffer, 0, saveStart);

	    if (saveStart > 0 && buffer[saveStart-1] == '\r')
	      seenCR = true;
	    char[] newBuffer;
	    int copyAmount = limit - saveStart;
	    if (copyAmount >= buffer.length)
	      newBuffer = new char [2 * buffer.length];
	    else
	      {
		newBuffer = buffer;
		if (saveStart == 0)
		  copyAmount = 0;
	      }
	    System.arraycopy (buffer, saveStart, newBuffer, 0, copyAmount);
	    buffer = newBuffer;

	    markPos -= saveStart;
	    pos -= saveStart;
	  }

	int readCount = buffer.length - pos;
	readCount = in.read (buffer, pos, readCount);
	readCount = readHook (buffer, pos, readCount);
	if (readCount <= 0)
	  {
	    limit = pos;
	    return -1;
	  }
	limit = pos + readCount;
	if (seenCR && buffer[0] == '\n')
	  {
	    lineNumber--;
	    if (pos == 0)
	      {
		pos++;
		return read();
	      }
	  }
      }

    int ch = buffer[pos++];
    if (ch == '\n')
      {
	lineStartPos = pos;
	if (convertCR && pos > 1 && buffer[pos-2] == '\r')
	  return read();
      }
    if (ch == '\r')
      {
	lineStartPos = pos;
	if (convertCR)
	  return '\n';
      }
    return ch;
  }

  public int read (char[] cbuf, int off, int len) throws java.io.IOException
  {
    if (len <= 0)
      return len;
    if (pos >= limit)
      {
	int ch = read();
	if (ch < 0)
	  return ch;
	pos--; // unread
      }
    if (limit - pos < len)
      len = limit - pos;
    lineStartPos = -1;
    System.arraycopy(buffer, pos, cbuf, off, len);
    pos += len;
    return len;
  }

  public int getLineNumber ()
  {
    return lineNumber + countLines (buffer, 0, pos);
  }

  public void setLineNumber (int lineNumber)
  {
    this.lineNumber = lineNumber - countLines (buffer, 0, pos);
  }

  public int getColumnNumber ()
  {
    if (lineStartPos < 0)
      {
	lineStartPos = 0;
	for (int i = 0;  i < pos; )
	  {
	    char ch = buffer[i++];
	    if (ch == '\n' || ch == '\r')
	      lineStartPos = i;
	  }
      }
    return pos - lineStartPos;
  }

  public boolean markSupported ()
  {
    return true;
  }

  public synchronized void mark (int readAheadLimit)
  {
    this.readAheadLimit = readAheadLimit;
    markPos = pos;
  }

  public void reset ()  throws IOException
  {
    if (readAheadLimit <= 0)
      throw new IOException ("mark invalid");
    if (markPos < lineStartPos)
      lineStartPos = -1;
    pos = markPos;
    readAheadLimit = 0;
  }

  public String readLine() throws IOException
  {
    int startPos = pos;
    int endPos;
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  {
	    endPos = pos;
	    break;
	  }
	if (ch == '\n')
	  {
	    endPos = pos - 1;
	    break;
	  }
      }
    return new String (buffer, startPos, endPos - startPos);
  }

  /** Skip forwards or backwards a number of characters. */
  public long skip(long n) throws IOException
  {
    long toSkip = n;
    if (n < 0)
      {
	toSkip += pos;
	if (toSkip < 0)
	  {
	    lineStartPos = 0;
	    toSkip = -pos;
	    pos = 0;
	    return toSkip;
	  }
	if (toSkip < lineStartPos)
	  lineStartPos = -1;
	pos = (int) toSkip;
	return n;
      }
    while (toSkip > 0 && read() >= 0)
      --toSkip;
    return n-toSkip;
  }

  public boolean ready () throws java.io.IOException
  {
    return pos < limit || in.ready();
  }

  public void skip () throws java.io.IOException
  {
    read();
  }

  static int countLines (char[] buffer, int off, int len)
  {
    int count = 0;
    char prev = '\0';
    for (int i = 0;  i < len;  i++)
      {
	char ch = buffer[i+off];
	if ((ch == '\n' && prev != '\r') || ch == '\r')
	  count++;
	prev = ch;
      }
    return count;
  }

  /* Move one character backwards. */
  public void unread ()
       throws java.io.IOException
  {
    if (pos == 0)
      throw new java.io.IOException("unread too much");
    if (pos == lineStartPos)
      lineStartPos = -1;
    pos--;
  }

  public int peek ()
       throws java.io.IOException
  {
    int oldLineStartPos = lineStartPos;
    int ch = read ();
    if (ch >= 0)
      {
	pos--;
	lineStartPos = oldLineStartPos;
      }
    return ch;
  }

}
