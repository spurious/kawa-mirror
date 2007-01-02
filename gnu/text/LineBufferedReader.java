// Copyright (c) 2004, 2007  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
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
  *
  * @author Per Bothner <bothner@cygnus.com>
  */

public class LineBufferedReader extends Reader
{
  public void close () throws IOException { in.close(); }

  // Not used by LineInputStreamReader subclass!
  protected Reader in;

  /** Default (initial buffer) size. */
  public final static int BUFFER_SIZE = 8192;

  /** The input buffer, containing the current line etc. */
  public char[] buffer;

  /** The current read position, as an index into buffer. */
  public int pos;

  /** The length of the valid (data-containing) part of the buffer. */
  public int limit;

  /** The high-water mark for pos, at a reset or line start. */
  int highestPos;

  public char readState = '\n';
  /** Return a character that indicates what we are currently reading.
    * Returns '\n' if we are not inside read; '\"' if reading a string;
    * '|' if inside a comment; '(' if inside a list; and
    * ' ' if otherwise inside a read. */
  public char getReadState () { return readState; }

  private int flags;

  // Notice the asymmetry in how "X\r\nY" is handled (assuming convertCR)..
  // When we read forward, the positions are 0, 1, 2, 4.
  // After seeing the '\r', we do not read ahead to look for a '\n'
  // because if we did, and there were no '\n', terminal input would hang.
  // The (lineNumber, lineStartPos) goes (0,0), (0,0), (0,0), (1,3).
  // However, the methods (getLineNumber(), getColumnNumber())
  // return the external values (0:0), (0:1), (1:0), (1:1).
  // When we move backwards, the positions are 4, 3, 1, 0.
  // This is because we want to stay within the same line.
  // The (lineNumber, lineStartPos) goes (1,3), (1,3), (0,0), (0,0).
  // For (getLineNumber(), getColumnNumber()) we get (1:1), (1:0), (0:1), (0:0)
  // which are the same as when we are moving forwards.
  // A nice bonus is that both skip_quick and unread_quick are trivial.

  /* If true in flags, convert "\r" and "\r\n" to '\n'. */
  private static final int CONVERT_CR = 1;

  /* If true in flags, may not re-allocate buffer. */
  private static final int USER_BUFFER = 2;

  /* If true in flags, char before start of buffer was '\r'. */
  private static final int PREV_WAS_CR = 4;

  /** If true in flags, we don't need to keep the whole line.
   * I.e. the application doesn't need to scan to the beginning of line. */
  private static final int DONT_KEEP_FULL_LINES = 8;

  /** Should we preserve the complete current line?
   * The default is true, but in some cases there can be a performance
   * improvement if we don't need to keep a long line when filling the buffer.
   */
  public void setKeepFullLines (boolean keep)
  {
    if (keep)
      flags &= ~DONT_KEEP_FULL_LINES;
    else
      flags |= DONT_KEEP_FULL_LINES;
  }

  /** True if CR and CRLF should be converted to LF. */
  public final boolean getConvertCR () { return (flags & CONVERT_CR) != 0; }

  public final void setConvertCR(boolean convertCR)
  {
    if (convertCR)
      flags |= CONVERT_CR;
    else
      flags &= ~CONVERT_CR;
  }

  /** The position that marks the start of the current or marked line.
    * If the {@code readAheadLimit > 0 && markPos < pos},
    * then it is the start of the line containing the {@code markPos}.
    * If we are at the end of a line, and have not started reading the next
    * one (and are therefore allowed by unread back to the old line),
    * the current line is still the old line; {@code lineStartPos} does not
    * get set to the new pos until we read/peek the first char of the new line.
    * If {@code lineStartPos < 0}, it means we went beyond the buffer maximum.
    */
  private int lineStartPos;

  Object name;

  /** The current line number (at position of lineStartPos). */
  protected int lineNumber;

  /** If mark has been called, and not invalidated, the read ahead limit.
    * Zero if mark has not been called, or had been invalidated
    * (due to either calling reset or excessive reading ahead). */
  protected int readAheadLimit = 0;

  /** The position of the mark (assuming {@code readAheadLinit > 0}).
    * (Garbage if {@code readAheadLimit <= 0}). */
  protected int markPos;

  public LineBufferedReader (InputStream in)
  {
    this.in = new InputStreamReader(in);
  }

  public LineBufferedReader (Reader in)
  {
    this.in = in;
  }

  /** A hook to allow sub-classes to perform some action at start of line.
    * Called just before the first character of the new line is read.
    * @param revisited true if we have read here before (i.e.
    *   we did a reset of unread() to get here)
    */
  public void lineStart (boolean revisited) throws java.io.IOException
  {
  }

  /** Called by {@code read()} when it needs its buffer filled.
    * Read characters into buffer, starting at off, for len.
    * Can assume that len > 0.  Only called if {@code pos>=limit}.
    * Return -1 if EOF, otherwise number of read chars.
    * This can be usefully overridden by sub-classes. */
  public int fill (int len) throws java.io.IOException
  {
    return in.read(buffer, pos, len);
  }

  private void clearMark ()
  {
    // Invalidate the mark.
    readAheadLimit = 0;
    // Need to maintain the lineStartPos invariant.
    int i = lineStartPos < 0 ? 0 : lineStartPos;
    for (;;)
      {
	if (++i >= pos)
	  break;
	char ch = buffer[i-1];
	if (ch == '\n'
	    || (ch == '\r' && (!getConvertCR() || buffer[i] != '\n')))
	  {
	    lineNumber++;
	    lineStartPos = i;
	  }
      
      }
  }

  /** Specify a buffer to use for the input buffer. */
  public void setBuffer (char[] buffer)
    throws java.io.IOException
  {
    if (buffer == null)
      {
	if (this.buffer != null)
	  {
	    buffer = new char[this.buffer.length];
	    System.arraycopy(this.buffer, 0, buffer, 0, this.buffer.length);
	    this.buffer = buffer;
	  }
	flags &= ~USER_BUFFER;
      }
    else
      {
	if (limit - pos > buffer.length)
	  throw new java.io.IOException("setBuffer - too short");
	flags |= USER_BUFFER;
	reserve (buffer, 0);
      }
  }

  /* Make sure there is enough space for more characters in buffer. */

  private void reserve (char[] buffer, int reserve)
    throws java.io.IOException
  {
    int saveStart;
    reserve += limit;
    if (reserve <= buffer.length)
      saveStart = 0;
    else
      {
	saveStart = pos;
	if (readAheadLimit > 0 && markPos < pos)
	  {
	    if (pos - markPos > readAheadLimit
		|| ((flags & USER_BUFFER) != 0
		    && reserve - markPos > buffer.length))
	      clearMark();
	    else
	      saveStart = markPos;
	  }

	reserve -= buffer.length;
	if (reserve <= saveStart
            && (saveStart <= lineStartPos
                || (flags & DONT_KEEP_FULL_LINES) != 0))
	  ;
	else if (reserve <= lineStartPos && saveStart > lineStartPos)
	  saveStart = lineStartPos;
	else if ((flags & USER_BUFFER) != 0)
	  saveStart -= (saveStart - reserve) >> 2;
	else
	  {
	    if (lineStartPos >= 0)
		saveStart = lineStartPos;
	    buffer = new char[2 * buffer.length];
	  }

	lineStartPos -= saveStart;
	limit -= saveStart;
	markPos -= saveStart;
	pos -= saveStart;
	highestPos -= saveStart;
      }
    if (limit > 0)
      System.arraycopy(this.buffer, saveStart, buffer, 0, limit);
    this.buffer = buffer;
  }

  public int read () throws java.io.IOException
  {
    char prev;
    if (pos > 0)
      prev = buffer[pos-1];
    else if ((flags & PREV_WAS_CR) != 0)
      prev = '\r';
    else if (lineStartPos >= 0)
      prev = '\n';
    else
      prev = '\0';
    if (prev == '\r' || prev == '\n')
      {
	if (lineStartPos < pos && (readAheadLimit == 0 || pos <= markPos))
	  {
	    lineStartPos = pos;
	    lineNumber++;
	  }
	boolean revisited = pos < highestPos;
	if (prev != '\n'
	    || (pos <= 1 ? (flags & PREV_WAS_CR) == 0 : buffer[pos-2] != '\r'))
	  {
	    lineStart(revisited);
	  }
	if (! revisited)
	  highestPos = pos + 1;  // Add one for this read().
      }

    if (pos >= limit)
      {
	if (buffer == null)
	  buffer = new char[BUFFER_SIZE];
	else if (limit == buffer.length)
	  reserve(buffer, 1);
	if (pos == 0)
	  {
	    if (prev == '\r')
	      flags |= PREV_WAS_CR;
	    else
	      flags &= ~PREV_WAS_CR;
	  }
	int readCount = fill(buffer.length - pos);
	if (readCount <= 0)
	  return -1;
	limit += readCount;
      }

    int ch = buffer[pos++];
    if (ch == '\n')
      {
	if (prev == '\r')
	  {
	    // lineNumber is the number of lines before lineStartPos.
	    // If lineStartPos is between '\r and '\n', we will count
	    // an extra line for the '\n', which gets the count off.
	    // Hence compensate.
	    if (lineStartPos == pos - 1)
	      {
		lineNumber--;
		lineStartPos--;
	      }
	    if (getConvertCR())
	      return read();
	  }
      }
    else if (ch == '\r')
      {
	if (getConvertCR())
	  return '\n';
      }
    return ch;
  }

  public int read (char[] cbuf, int off, int len) throws java.io.IOException
  {
    // Same logic as in skip(n), when n>0.
    int ch;
    if (pos >= limit)
      ch = '\0';
    else if (pos > 0)
      ch = buffer[pos-1];
    else if ((flags & PREV_WAS_CR) != 0 || lineStartPos >= 0)
      ch = '\n';
    else
      ch = '\0';
    int to_do = len;
    while (to_do > 0)
      {
	if (pos >= limit || ch == '\n' || ch == '\r')
	  {
	    // Return if there is no more in the input buffer, and we got
	    // at least one char.  This is desirable for interactive input.
	    if (pos >= limit && to_do < len)
	      return len - to_do;
	    ch = read();
	    if (ch < 0)
	      {
		len -= to_do;
		return len <= 0 ? -1 : len;
	      }
	    cbuf[off++] = (char) ch;
	    to_do--;
	  }
	else
	  {
	    int p = pos;
	    int lim = limit;
	    if (to_do < lim - p)
	      lim = p + to_do;
	    while (p < lim)
	      {
		ch = buffer[p];
		// For simplicity and correctness we defer handling of
		// newlines (including previous character) to read().
		if (ch == '\n' || ch == '\r')
		  break;
		cbuf[off++] = (char) ch;
		p++;
	      }
	    to_do -= p - pos;
	    pos = p;
	  }
      }
    return len;
  }

  /** Should return a URI or a String. */
  public Object getURI ()
  {
    return name;
  }

  public String getName ()
  {
    return name == null ? null : name.toString();
  }

  public void setName (Object name)
  {
    this.name = name;
  }

  /** Get the current line number.
    * The "first" line is number number 0. */
  public int getLineNumber ()
  {
    int lineno = lineNumber;
    if (readAheadLimit == 0) // Normal, fast case:
      {
	if (pos > 0 && pos > lineStartPos)
	  {
	    char prev = buffer[pos-1];
	    if (prev == '\n' || prev == '\r')
	      lineno++;
	  }
      }
    else
      lineno += countLines(buffer, lineStartPos < 0 ? 0 : lineStartPos, pos);
    return lineno;
  }

  public void setLineNumber (int lineNumber)
  {
    this.lineNumber += lineNumber - getLineNumber();
  }

  public void incrLineNumber (int lineDelta, int lineStartPos)
  {
    lineNumber += lineDelta;
    this.lineStartPos = lineStartPos;
  }

  /** Return the current (zero-based) column number. */
  public int getColumnNumber ()
  {
    if (pos > 0)
      {
	char prev = buffer[pos-1];
	if (prev == '\n' || prev == '\r')
	  return 0;
      }
    if (readAheadLimit <= 0) // Normal, fast case:
      return pos - lineStartPos;

    // Somebody did a mark().  Thus lineStartPos is not necessarily the
    // start of the current line, so we have to search.
    int start = lineStartPos < 0 ? 0 : lineStartPos;
    for (int i = start;  i < pos; )
      {
	char ch = buffer[i++];
	if (ch == '\n' || ch == '\r')
	  start = i;
      }
    int col = pos - start;
    if (lineStartPos < 0)
      col -= lineStartPos;
    return col;
  }

  public boolean markSupported ()
  {
    return true;
  }

  public synchronized void mark (int readAheadLimit)
  {
    if (this.readAheadLimit > 0)
      clearMark();
    this.readAheadLimit = readAheadLimit;
    markPos = pos;
  }

  public void reset ()  throws IOException
  {
    if (readAheadLimit <= 0)
      throw new IOException ("mark invalid");
    if (pos > highestPos)
      highestPos = pos;
    pos = markPos;
    readAheadLimit = 0;
  }

  /** Read a line.
   * If mode is 'I' ("ignore") ignore delimiters.
   * If mode is 'P' ("peek") leave delimiter in input stream.
   * If mode is 'A' ("append") append delimiter to result.
   */

  public void readLine(StringBuffer sbuf, char mode)
    throws IOException
  {
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  return;
        int start = --pos;
        while (pos < limit)
          {
            ch = buffer[pos++];
	    if (ch == '\r' || ch == '\n')
              {
                sbuf.append(buffer, start, pos - 1 - start);
                if (mode == 'P')
                  {
                    pos--;
                    return;
                  }
                if (getConvertCR () || ch == '\n')
                  {
                    if (mode != 'I')
                      sbuf.append('\n');
                  }
                else
                  {
                    if (mode != 'I')
                      sbuf.append('\r');
                    ch = read();
                    if (ch == '\n')
                      {
                        if (mode != 'I')
                          sbuf.append('\n');
                      }
                    else if (ch >= 0)
                      unread_quick();
                  }
                return;
              }
          }
	sbuf.append(buffer, start, pos - start);
      }
  }

  public String readLine() throws IOException
  {
    int ch = read();
    if (ch < 0)
      return null;
    if (ch == '\r' || ch == '\n')
      return "";
    int start = pos - 1;
    while (pos < limit)
      {
	ch = buffer[pos++];
	if (ch == '\r' || ch == '\n')
          {
            int end = pos - 1;
            if (ch != '\n' && ! getConvertCR())
              {
                if (pos >= limit)
                  {
                    pos--;
                    break;
                  }
                if (buffer[pos] == '\n')
                  pos++;
              }
            return new String(buffer, start, end - start);
          }
      }
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append(buffer, start, pos - start);
    readLine(sbuf, 'I');
    return sbuf.toString();
  }

  /** Skip forwards or backwards a number of characters. */
  public int skip(int n) throws IOException
  {
    if (n < 0)
      {
	int to_do = -n;
	for (; to_do > 0 && pos > 0;  to_do--)
	  unread();
	return n + to_do;
      }
    else
      {
	// Same logic as in read(char[],int,int).
	int to_do = n;
	int ch;
	if (pos >= limit)
	  ch = '\0';
	else if (pos > 0)
	  ch = buffer[pos-1];
	else if ((flags & PREV_WAS_CR) != 0 || lineStartPos >= 0)
	  ch = '\n';
	else
	  ch = '\0';
	while (to_do > 0)
	  {
	    if (ch == '\n' || ch == '\r' || pos >= limit)
	      {
		ch = read();
		if (ch < 0)
		  return n - to_do;
		to_do--;
	      }
	    else
	      {
		int p = pos;
		int lim = limit;
		if (to_do < lim - p)
		  lim = p + to_do;
		while (p < lim)
		  {
		    ch = buffer[p];
		    // For simplicity and correctness we defer handling of
		    // newlines (including previous character) to read().
		    if (ch == '\n' || ch == '\r')
		      break;
		    p++;
		  }
		to_do -= p - pos;
		pos = p;
	      }
	  }
	return n;
      }
  }

  public boolean ready () throws java.io.IOException
  {
    return pos < limit || in.ready();
  }

  /** Same as skip(), but assumes previous command was a non-EOF peek(). */
  public final void skip_quick () throws java.io.IOException
  {
    pos++;
  }

  public void skip () throws java.io.IOException
  {
    read();
  }

  static int countLines (char[] buffer, int start, int limit)
  {
    int count = 0;
    char prev = '\0';
    for (int i = start;  i < limit;  i++)
      {
	char ch = buffer[i];
	if ((ch == '\n' && prev != '\r') || ch == '\r')
	  count++;
	prev = ch;
      }
    return count;
  }

  /** Skips the rest of the current line, including the line terminator. */
  public void skipRestOfLine ()
       throws java.io.IOException
  {
    for (;;)
      {
        int c = read();
        if (c < 0)
          return;
        if (c == '\r')
          {
            c = read();
            if (c >= 0 && c != '\n')
              unread();
            break;
          }
        else if (c == '\n')
          break;
      }
  }

  /* Move one character backwards. */
  public void unread ()
       throws java.io.IOException
  {
    if (pos == 0)
      throw new java.io.IOException("unread too much");
    pos--;
    char ch = buffer[pos];
    if (ch == '\n' || ch == '\r')
      {
	if (pos > 0 && ch == '\n' && getConvertCR() && buffer[pos-1] == '\r')
	  pos--;
	if (pos < lineStartPos)
	  {
	    lineNumber--;
	    int i;
	    for (i = pos;  i > 0; )
	      {
		ch = buffer[--i];
		if (ch == '\r' || ch == '\n')
		  {
		    i++;
		    break;
		  }
	      }
	    lineStartPos = i;
	  }
      }
  }

  /** Same as unread, but only allowed after non-EOF-returning read().
   * Also allowed after an intervening peek(), but only if the read()
   * did not return '\r' or '\n'. */
  public void unread_quick ()
  {
    pos--;
  }

  public int peek ()
       throws java.io.IOException
  {
    if (pos < limit && pos > 0)
      {
	char ch = buffer[pos - 1];
	if (ch != '\n' && ch != '\r')
	  {
	    ch = buffer[pos];
	    if (ch == '\r' && getConvertCR())
	      ch = '\n';
	    return ch;
	  }
      }
    int c = read ();
    if (c >= 0)
      unread_quick();
    return c;
  }
}
