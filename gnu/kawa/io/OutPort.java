package gnu.kawa.io;
import java.io.*;
import gnu.mapping.Environment;
import gnu.mapping.ThreadLocation;
import gnu.kawa.format.Printable;
import gnu.kawa.format.AbstractFormat;
import gnu.lists.*;

/**
 * An extended PrintWriter.
 */

public class OutPort extends PrintConsumer implements Printable
{
    /** Output gets forwarded to the value of this field.
     * It is initially a PrettyWriter, but other PrintConsumer object
     * can be interjected for custom formatting or other processing. */
    PrintConsumer formatter;

  Path path;
  protected Writer base;
  static final int FLUSH_ON_FINALIZE = 1;
  static final int CLOSE_ON_FINALIZE = 2;
  static final int IS_CLOSED = 4;
  static final int IS_DOMTERM = 8;
    int flags;

  // To keep track of column-numbers, we use a helper class.
  // Otherwise, it is too painful, as there is no documented
  // interface that would allow PrintWriter to be cleanly extended ...
  // The helper class also lets us make transparent use of WriterManager.
  protected PrettyWriter bout;

  /** An index into the WriterManager's internal table.
   * The value zero means it is unregistered. */
  protected WriterManager.WriterRef unregisterRef;

  protected OutPort(Writer base, PrettyWriter out, boolean autoflush)
  {
    super((Consumer) out, autoflush);
    this.bout = out;
    this.formatter = bout;
    this.base = base;
    if (closeOnExit())
      unregisterRef = WriterManager.instance.register(this);
  }

  protected OutPort (OutPort out, boolean autoflush)
  {
    this(out, out.bout, autoflush);
  }

  protected OutPort (Writer out, boolean autoflush)
  {
    this(out,
         (out instanceof OutPort ? ((OutPort) out).bout
          : new PrettyWriter(out, true)),
         autoflush);
  }

  public OutPort(Writer base, boolean printPretty, boolean autoflush)
  {
    this(base, new PrettyWriter(base, printPretty), autoflush);
  }

  public OutPort(Writer base, boolean printPretty,
		 boolean autoflush, Path path)
  {
    this(base, new PrettyWriter(base, printPretty), autoflush);
    this.path = path;
  }

  public OutPort (OutputStream out)
  {
    this (out, null);
  }

  public OutPort (OutputStream out, Path path)
  {
    this(new OutputStreamWriter(out), true, path);
  }

  public OutPort (Writer out)
  {
    this(out,
         out instanceof OutPort ? ((OutPort) out).bout
         : new PrettyWriter(out, false),
         false);
  }

  public OutPort (Writer base, Path path)
  {
    this(base, false, false);
    this.path = path;
  }

  public OutPort (Writer base, boolean autoflush, Path path)
  {
    this (base, false, autoflush);
    this.path = path;
  }

    static BinaryOutPort outInitial
        = BinaryOutPort.makeStandardPort(System.out, "/dev/stdout");
    static { outInitial.flags = FLUSH_ON_FINALIZE; }

    private static BinaryOutPort errInitial
        = BinaryOutPort.makeStandardPort(System.err, "/dev/stderr");
    static { errInitial.flags = FLUSH_ON_FINALIZE; }

    public static BinaryOutPort getSystemOut() { return outInitial; }
    public static BinaryOutPort getSystemErr() { return errInitial; }

  public static final ThreadLocation<gnu.kawa.io.OutPort> outLocation
    = new ThreadLocation<gnu.kawa.io.OutPort>("out-default");
  static { outLocation.setGlobal(outInitial); }
  public static final ThreadLocation<gnu.kawa.io.OutPort> errLocation
    = new ThreadLocation<gnu.kawa.io.OutPort>("err-default");
  static { errLocation.setGlobal(errInitial); }
  static public OutPort outDefault ()
  {
    return (OutPort) outLocation.get();
  }

  static public void setOutDefault (OutPort o)
  {
    outLocation.set(o);
  }

  static public OutPort errDefault ()
  {
    return (OutPort) errLocation.get();
  }

  static public void setErrDefault (OutPort e)
  {
    errLocation.set(e);
  }

    public boolean isPrettyPrinting() { return bout.isPrettyPrinting(); }

    public static OutPort openFile(Object fname)
        throws java.io.IOException {
        return openFile(fname, Environment.user().get("port-char-encoding"));
    }

    public static OutPort openFile(Object fname, Object conv)
        throws java.io.IOException {
        Path path = Path.valueOf(fname);
        java.io.OutputStream strm = path.openOutputStream();
        strm = new java.io.BufferedOutputStream(strm);
        // Do we need to wrap the OutputStreamWriter in a BufferedWriter?
        // There is buffering in PrettyWriter, but not always.  FIXME.
        OutPort op = conv == Boolean.FALSE
            ? new BinaryOutPort(strm, path)
            : new OutPort(conv == null || conv == Boolean.TRUE
                          ? new OutputStreamWriter(strm)
                          : new OutputStreamWriter(strm, conv.toString()),
                          path);
        op.flags = CLOSE_ON_FINALIZE;
        return op;
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

  protected static final boolean isWordChar(char ch)
  {
    return Character.isJavaIdentifierPart(ch) || ch == '-' || ch == '+';
  }

  @Override
  public void print(int v)
  {
    formatter.writeInt(v);
  }

  @Override
  public void print(long v)
  {
    formatter.writeLong(v);
  }

  @Override
  public void print(double v)
  {
    formatter.writeDouble(v);
  }

  @Override
  public void print(float v)
  {
    formatter.writeFloat(v);
  }

  @Override
  public void print(boolean v)
  {
    formatter.writeBoolean(v);
  }

  @Override
  public void print(String v)
  {
    write(v == null ? "(null)" : v);
  }

  @Override
  public void print(Object v)
  {
    formatter.writeObject(v);
  }

  public void print (Consumer out)
  {
    out.write("#<output-port");
    if (path != null)
      {
	out.write(' ');
	out.write(path.toString());
      }
    out.write('>');
  }

    @Override
    public void startDocument() {
        formatter.startDocument();
    }

    @Override
    public void endDocument() {
        formatter.endDocument();
    }

   @Override
  public void startElement (Object type)
  {
    formatter.startElement(type);
  }

  @Override
  public void endElement ()
  {
    formatter.endElement();
  }

  /** Write a attribute for the current element.
   * This is only allowed immediately after a startElement. */
  @Override
  public void startAttribute (Object attrType)
  {
    formatter.startAttribute(attrType);
  }

  /** No more attributes in this element. */
  @Override
  public void endAttribute()
  {
    formatter.endAttribute();
  }

    public void write(char[] str, int start, int count) {
        formatter.write(str, start, count);
    }

    public void write(String str) {
        formatter.write(str, 0, str.length());
    }

    public void write(String str, int start, int count) {
        formatter.write(str, start, count);
    }

    public void write(CharSequence str, int start, int count) {
        formatter.write(str, start, count);
    }

    public void writeObject(Object v) {
        formatter.writeObject(v);
    }

    public void writeComment(char[] chars, int offset, int length) {
        formatter.writeComment(chars, offset, length);
    }

    public void writeProcessingInstruction(String target, char[] content,
                                           int offset, int length) {
        formatter.writeProcessingInstruction(target, content, offset, length);
    }

    public void writeCDATA(char[] chars, int offset, int length) {
        formatter.writeCDATA(chars, offset, length);
    }

    public void beginEntity (Object baseUri) {
        formatter.beginEntity(baseUri);
    }
    public void endEntity() {
        formatter.endEntity();
    }

    /** Note the end of a "word".  See {@link #writeWordStart}. */
  public void writeWordEnd ()
  {
    formatter.writeWordEnd();
  }

  /** Maybe write a word-separating space.
   * Specifically, write a space if the previous output
   * was {@link #writeWordEnd}.  Otherwise, do nothing.
   */
  public void writeWordStart ()
  {
    formatter.writeWordStart();
  }

    public Object pushFormat(AbstractFormat format) {
        Object old = formatter;
        formatter = format.makeConsumer(formatter);
        return old;
    }
    public void popFormat(Object old) {
        formatter = (PrintConsumer) old;
    }

  public void freshLine()
  {
    if (! atLineStart())
      println();
  }

  /** Get zero-based column number or -1 for unknown. */
  public int getColumnNumber ()
  {
    return bout.getColumnNumber();
  }

  public void setColumnNumber (int column)
  {
    bout.setColumnNumber(column);
  }

    public boolean atLineStart() {
        return bout.atLineStart();
    }

    void flushBuffer() {
        bout.forcePrettyOutput();
    }

  public void clearBuffer ()
  {
    bout.clearBuffer();
  }

  /** Flush and close this local Writer, but not underlying Writers. */
  public void closeThis()
  {
    try
      {
        if (! (base instanceof OutPort && ((OutPort) base).bout == bout)) {
          bout.closeThis();
          base = null;
          out = null;
        }
      }
    catch (IOException ex)
      {
        setError();
      }
    WriterManager.instance.unregister(unregisterRef);
    unregisterRef = null;
  }

    public boolean isOpen() { return (flags & IS_CLOSED) == 0; }
    public boolean isDomTerm() { return (flags & IS_DOMTERM) != 0; }
    public void setDomTerm(boolean v) {
        if (v) flags |= IS_DOMTERM;
        else flags &= ~IS_DOMTERM;
    }

    /** Return an OutPort equivalent to the argumement for text output.
     * I.e. writing CharSequences or characters to either is the same.
     * Used to optimize process output.
     */
    public static OutPort getPassThroughOutPort(Consumer out) {
        OutPort port = null;
        for (;;) {
            if (out instanceof OutPort) {
                port = (OutPort) out;
                PrintConsumer formatter = port.formatter;
                if (formatter instanceof PrettyWriter)
                    return port;
                out = formatter;
            } else if (out instanceof AbstractFormat.FormatConsumer) {
                AbstractFormat.FormatConsumer fcons = (AbstractFormat.FormatConsumer) out;
                if (!  fcons.getFormat().textIsCopied())
                    return null;
                out = fcons.getBase();
            }
            else
                return port;
        }
    }

  @Override
  public void close()
  {
    try
      {
        if (base instanceof OutPort && ((OutPort) base).bout == bout) {
          base.close();
          base = null;
        }
        else if (out != null) {
          out.close();
          out = null;
        }
      }
    catch (IOException ex)
      {
        setError();
      }
    WriterManager.instance.unregister(unregisterRef);
    unregisterRef = null;
    flags = IS_CLOSED;
  }

  /** True if the port should be automatically closed on exit.
   * (If so, it will be registered by WriterManager. */
  protected boolean closeOnExit ()
  {
    return true;
  }

  public void finalize ()
  {
    if ((flags & FLUSH_ON_FINALIZE) != 0)
      flush();
    if ((flags & CLOSE_ON_FINALIZE) != 0)
      close();
    else
      closeThis();
  }

  public static void runCleanups ()
  {
    WriterManager.instance.run();
  }

  public void startLogicalBlock (String prefix, boolean perLine,
				 String suffix)
  {
    bout.startLogicalBlock(prefix, perLine, suffix);
  }

    public void startLogicalBlock(String prefix, String suffix,
                                  int indent) {
        synchronized(lock) {
            bout.startLogicalBlock(prefix, false, suffix);
            bout.addIndentation(prefix == null ? indent
                                :  indent - prefix.length(),
                                false);
        }
    }

  public void endLogicalBlock (String suffix)
  {
    bout.endLogicalBlock(suffix);
  }

    public void writeBreak(int kind) {
        bout.writeBreak(kind);
    }

    /** Write a new-line if needed, space otherwise. */
    public void writeSpace(int kind) {
        synchronized (lock) {
            write(' ');
            writeBreak(kind);
        }
    }

    @Override
    public void setIndentation(int amount, boolean current) {
        bout.addIndentation(amount, current);
    }
}
