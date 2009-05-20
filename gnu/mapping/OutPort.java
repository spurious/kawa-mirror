package gnu.mapping;
import java.io.*;
import gnu.text.*;
import gnu.lists.*;

/**
 * An extended PrintWriter.
 */

public class OutPort extends PrintConsumer implements Printable
{
  Path path;
  private Writer base;

  // To keep track of column-numbers, we use a helper class.
  // Otherwise, it is too painful, as there is no documented
  // interface that would allow PrintWriter to be cleanly extended ...
  // The helper class also lets us make transparent use of WriterManager.
  protected PrettyWriter bout;

  /** An index into the WriterManager's internal table.
   * The value zero means it is unregistered. */
  protected Object unregisterRef;
  
  protected OutPort(Writer base, PrettyWriter out, boolean autoflush)
  {
    super(out, autoflush);
    this.bout = out;
    this.base = base;
    if (closeOnExit())
      unregisterRef = WriterManager.instance.register(out);
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

  public boolean printReadable;

  static OutPort outInitial = new OutPort (new LogWriter (new BufferedWriter(new OutputStreamWriter(System.out))), true, true, Path.valueOf("/dev/stdout"));

  private static OutPort errInitial = new OutPort (new LogWriter(new OutputStreamWriter(System.err)), true, true, Path.valueOf("/dev/stderr"));

  public static final ThreadLocation outLocation
    = new ThreadLocation("out-default");
  static { outLocation.setGlobal(outInitial); }
  public static final ThreadLocation errLocation
    = new ThreadLocation("err-default");
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

  public static OutPort openFile(Object fname)
    throws java.io.IOException
  {
      Object conv = Environment.user().get("port-char-encoding");
      Path path = Path.valueOf(fname);
      java.io.OutputStream strm = path.openOutputStream();
      strm = new java.io.BufferedOutputStream(strm);
      java.io.Writer wr;
      if (conv == null || conv == Boolean.TRUE)
	wr = new java.io.OutputStreamWriter(strm);
      else
	{
	  if (conv == Boolean.FALSE)
	    conv = "8859_1";
	  wr = new java.io.OutputStreamWriter(strm, conv.toString());
	}
      return new OutPort(wr, path);
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

  //  java.text.FieldPosition fieldPosition;

  /** If non-null, use this to print numbers. */
  java.text.NumberFormat numberFormat;

  public AbstractFormat objectFormat;

  public void print(int v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format((long) v));
  }

  public void print(long v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format(v));
  }

  public void print(double v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format(v));
  }

  public void print(float v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format((double) v));
  }

  public void print(boolean v)
  {
    if (objectFormat == null)
      super.print(v);
    else
      objectFormat.writeBoolean(v, this);
  }

  public void print(String v)
  {
    write(v == null ? "(null)" : v);
  }

  public void print(Object v)
  {
    if (objectFormat != null)
      objectFormat.writeObject(v, this);
    else if (v instanceof Consumable)
      ((Consumable) v).consume(this);
    else
      super.print(v == null ? "null" : v);
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

  public void startElement (Object type)
  {
    if (objectFormat != null)
      objectFormat.startElement(type, this);
    else
      {
	print('(');
	print(type);
      }
  }

  public void endElement ()
  {
    if (objectFormat != null)
      objectFormat.endElement(this);
    else
      print(')');
  }

  /** Write a attribute for the current element.
   * This is only allowed immediately after a startElement. */
  public void startAttribute (Object attrType)
  {
    if (objectFormat != null)
      objectFormat.startAttribute(attrType, this);
    else
      {
        print(' ');
        print(attrType);
        print(": ");
      }
  }

  /** No more attributes in this element. */
  public void endAttribute()
  {
    if (objectFormat != null)
      objectFormat.endAttribute(this);
    else
      print(' ');
  }

  /** Note the end of a "word".  See {@link #writeWordStart}. */
  public void writeWordEnd ()
  {
    bout.writeWordEnd();
  }

  /** Maybe write a word-separating space.
   * Specifically, write a space if the previous output
   * was {@link #writeWordEnd}.  Otherwise, do nothing.
   */
  public void writeWordStart ()
  {
    bout.writeWordStart();
  }

  public void freshLine()
  {
    int col = bout.getColumnNumber();
    if (col != 0)
      println();
  }

  public int getColumnNumber ()
  {
    return bout.getColumnNumber();
  }

  public void setColumnNumber (int column)
  {
    bout.setColumnNumber(column);
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
        if (! (base instanceof OutPort && ((OutPort) base).bout == bout))
          bout.closeThis();
      }
    catch (IOException ex)
      {
        setError();
      }
    WriterManager.instance.unregister(unregisterRef);
  }

  public void close()
  {
    try
      {
        if (base instanceof OutPort && ((OutPort) base).bout == bout)
          base.close();
        else
          out.close();
      }
    catch (IOException ex)
      {
        setError();
      }
    WriterManager.instance.unregister(unregisterRef);
  }

  /** True if the port should be automatically closed on exit.
   * (If so, it will be registered by WriterManager. */
  protected boolean closeOnExit ()
  {
    return true;
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

  public void startLogicalBlock (String prefix, String suffix, int indent)
  {
    bout.startLogicalBlock(prefix, false, suffix);
    bout.addIndentation(prefix == null ? indent :  indent - prefix.length(),
			false);
  }

  public void endLogicalBlock (String suffix)
  {
    bout.endLogicalBlock(suffix);
  }

  public void writeBreak(int kind)
  {
    bout.writeBreak(kind);
  }

  public void writeSpaceLinear()
  {
    write(' ');
    writeBreak(PrettyWriter.NEWLINE_LINEAR);
  }

  /** Write a new-line iff the containing section cannot be printed
   * on one line.  Either all linear-style newlines in a logical
   * block becomes spaces (if it all fits in a line), or none
   * of them do. */
  public void writeBreakLinear()
  {
    writeBreak(PrettyWriter.NEWLINE_LINEAR);
  }

  /** Write a new-line if needed, space otherwise. */
  public void writeSpaceFill()
  {
    write(' ');
    writeBreak(PrettyWriter.NEWLINE_FILL);
  }

  public void writeBreakFill()
  {
    writeBreak(PrettyWriter.NEWLINE_FILL);
  }

  public void setIndentation(int amount, boolean current)
  {
    bout.addIndentation(amount, current);
  }
}
