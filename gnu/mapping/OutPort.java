package gnu.mapping;
import java.io.*;

/**
 * An extended PrintWriter.
 */

public class OutPort extends PrintWriter implements Printable
{
  String name;
  private Writer base;

  // To keep track of column-numbers, we use a helper class.
  // Otherwise, it is too painful, as there is no documented
  // interface that would allow PrintWriter to be cleanly extended ...
  BufferedPort bout;

  OutPort(Writer base, BufferedPort out, boolean autoflush)
  {
    super(out, autoflush);
    this.bout = out;
    this.base = base;
  }

  public OutPort(Writer base, int bufsize, boolean autoflush)
  {
    this(base, new BufferedPort(base, bufsize), autoflush);
  }

  public OutPort (OutputStream out)
  {
    this (out, null);
  }

  public OutPort (OutputStream out, String name)
  {
    this(new OutputStreamWriter(out), true, name);
  }

  public OutPort (Writer out)
  {
    this(out, 512, false);
  }

  public OutPort (Writer base, String name)
  {
    this(base, 512, false);
    this.name = name;
  }

  public OutPort (Writer base, boolean autoflush, String name)
  {
    this (base, 512, autoflush);
    this.name = name;
  }

  public boolean printReadable;

  // For now, these are static.  They should probably be thread-local.
  private static OutPort outInitial = new OutPort (new LogWriter (new BufferedWriter(new OutputStreamWriter(System.out))), true, "<stdout>");
  private static OutPort out = outInitial;

  private static OutPort errInitial = new OutPort (new LogWriter(new OutputStreamWriter(System.err)), true, "<stderr>");
  private static OutPort err = errInitial;

  static public OutPort outDefault ()
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      return ((Future) thread).out;
    return out;
  }

  static public void setOutDefault (OutPort o)
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      ((Future) thread).out = o;
    else
      out = o;
  }

  static public OutPort errDefault ()
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      return ((Future) thread).err;
    return err;
  }

  static public void setErrDefault (OutPort e)
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      ((Future) thread).err = e;
    else
      err = e;
  }

  public void echo (char[] buf, int off, int len)  throws java.io.IOException
  {
    if (base instanceof LogWriter)
      ((LogWriter)base).echo(buf, off, len);
  }

  static Writer logFile;

  public static void closeLogFile ()  throws java.io.IOException
  {
    if (logFile != null)
      {
	logFile.close();
	logFile = null;
      }
    if (outInitial.base instanceof LogWriter)
      ((LogWriter)outInitial.base).setLogFile((Writer) null);
    if (errInitial.base instanceof LogWriter)
      ((LogWriter)errInitial.base).setLogFile((Writer) null);
  }

  public static void setLogFile (String name)  throws java.io.IOException
  {
    if (logFile != null)
      closeLogFile();
    logFile = new PrintWriter(new BufferedWriter(new FileWriter(name)));
    if (outInitial.base instanceof LogWriter)
      ((LogWriter)outInitial.base).setLogFile(logFile);
    if (errInitial.base instanceof LogWriter)
      ((LogWriter)errInitial.base).setLogFile(logFile);
  }

  /*
  public void closeLogFile ()  throws java.io.IOException
  {
    if (base instanceof LogWriter)
      ((LogWriter)base).closeLogFile();
  }

  public void setLogFile (String name)  throws java.io.IOException
  {
    if (base instanceof LogWriter)
      ((LogWriter)base).setLogFile(name);
  }
  */

  /**
   * Write a character value to a byte-stream.
   * The default transation generates UTF-8 multi-bytes.
   * We support character values above 0xFFFF for future extension.
   */
  public void writeChar (int i)
  {
    write (i);
  }

  public void writeSchemeObject (Object obj, boolean readable)
  {
    boolean saveReadable = printReadable;
    try
      {
	printReadable = readable;
	SFormat.print (obj, this);
      }
    finally
      {
	printReadable = saveReadable;
      }
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<output-port");
    if (name != null)
      {
	ps.print (' ');
	ps.print (name);
      }
    ps.print ('>');
  }

  public int getColumnNumber ()
  {
    return bout.getColumnNumber();
  }
}

/** Not yet used ... for future pretty-printing. */

class Break
{
  /** Text to emit if we do not break the line here.
   * Typically a space. */
  String textNoBreak;

  /** Text to emit before the break, if we break the line here.
   * Typically empty. */
  String textAfterBreak;

  /** Text to emit after the break, if we break the line here.
   * Does not include indentation inherited from the outer group.
   * Typically empty. */
  String textBeforeBreak;

  int kind;

  /** If kind==FORCED_BREAK, always break the line. */
  final static int FORCED_BREAK = 0;

  /** If kind==LINEAR_BREAK, break the line here if it needs any breaks. */
  final static int LINEAR_BREAK = 1;

  /** If kind==LINEAR_BREAK, break the line here if next word will not fit. */
  final static int FILL_BREAK = 2;
}

/** A Writer with buffering and that tracks column numbers.
 * (Does not handle TABs specially - probably should.)
 * Should move to a separate file, and maybe rename, but deferring that
 * until more general output framework (e.g. pretty-printing).
 */

class BufferedPort extends FilterWriter
{
  char[] buffer;

  /** First used (unflushed) position in buffer. */
  int bufStartPos = 0;

  /** Next available position in buffer. */
  int bufWritePos = 0;

  /** Column number (0-origin) corresponding to bufStartPos. */
  int startColumn = 0;

  BufferedPort(Writer out, int bufsize)
  {
    super(out);
    buffer = new char[bufsize];
  }

  public void write (int ch)
    throws IOException
  {
    if (bufWritePos >= buffer.length)
      {
	flushLocal();
      }
    buffer[bufWritePos++] = (char) ch;
  }

  public void write (char[] chars, int offset, int count)
    throws IOException
  {
    while (count > 0)
      {
	if (bufWritePos + count > buffer.length)
	  flushLocal();
	int can_do = buffer.length - bufWritePos;
	if (can_do > count)
	  can_do = count;
	if (can_do == 0)
	  throw new Error("can't do anything!");
	System.arraycopy(chars, offset, buffer, bufWritePos, can_do);
	bufWritePos += can_do;
	offset += can_do;
	count -= can_do;
      }
  }

  public void write (String str, int offset, int count)
    throws IOException
  {
    while (count > 0)
      {
	if (bufWritePos + count > buffer.length)
	  flushLocal();
	int can_do = buffer.length - bufWritePos;
	if (can_do > count)
	  can_do = count;
	if (can_do == 0)
	  throw new Error("can't do anything! buflen:"+buffer.length+" bwpos:"+bufWritePos);
	str.getChars(offset, offset + can_do, buffer, bufWritePos);
	bufWritePos += can_do;
	offset += can_do;
	count -= can_do;
      }
  }

  // FIXME:  This should have a different name.  Check java.io.*.
  void flushLocal()
    throws IOException
  {
    if (out == null)
      {
        char[] newBuffer = new char[2 * buffer.length];
        System.arraycopy(buffer, 0, newBuffer, 0, bufWritePos);
        buffer = newBuffer;
        return;
      }
    int i = bufWritePos;
    for (;;)
      {
	if (--i < bufStartPos)
	  {
	    startColumn += bufWritePos - bufStartPos;
	    break;
	  }
	char ch = buffer[i];
	if (ch == '\n' || ch == '\r')
	  {
	    startColumn = bufWritePos - i;
	    break;
	  }
      }
    out.write(buffer, bufStartPos, bufWritePos - bufStartPos);
    bufWritePos = 0;
    bufStartPos = 0;
  }

  public void flush()
    throws IOException
  {
    if (out == null)
      return;
    flushLocal();
    out.flush();
  }

  public int getColumnNumber ()
  {
    int i = bufWritePos;
    for (;;)
      {
	if (--i < bufStartPos)
	  return startColumn + bufWritePos - bufStartPos;
	char ch = buffer[i];
	if (ch == '\n' || ch == '\r')
	  return bufWritePos - i;
      }
  }

  public void close()
    throws IOException
  {
    if (out != null)
      {
        flushLocal();
        out.close();
        out = null;
      }
    buffer = null;
  }
}
