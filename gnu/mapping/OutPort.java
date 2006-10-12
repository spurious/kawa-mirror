package gnu.mapping;
import java.io.*;
import gnu.text.*;
import gnu.lists.*;

/**
 * An extended PrintWriter.
 */

public class OutPort extends PrintConsumer implements Printable
{
  Object name;
  private Writer base;

  // To keep track of column-numbers, we use a helper class.
  // Otherwise, it is too painful, as there is no documented
  // interface that would allow PrintWriter to be cleanly extended ...
  // The helper class also lets us make transparent use of WriterManager.
  protected PrettyWriter bout;

  /** An index into the WriterManager's internal table.
   * The value zero means it is unregistered. */
  protected int index;
  
  protected OutPort(Writer base, PrettyWriter out, boolean autoflush)
  {
    super(out, autoflush);
    this.bout = out;
    this.base = base;
    if (closeOnExit())
      index = WriterManager.instance.register(out);
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
		 boolean autoflush, Object name)
  {
    this(base, new PrettyWriter(base, printPretty), autoflush);
    this.name = name;
  }

  public OutPort (OutputStream out)
  {
    this (out, null);
  }

  public OutPort (OutputStream out, Object name)
  {
    this(new OutputStreamWriter(out), true, name);
  }

  public OutPort (Writer out)
  {
    this(out,
         out instanceof OutPort ? ((OutPort) out).bout
         : new PrettyWriter(out, false),
         false);
  }

  public OutPort (Writer base, Object name)
  {
    this(base, false, false);
    this.name = name;
  }

  public OutPort (Writer base, boolean autoflush, Object name)
  {
    this (base, false, autoflush);
    this.name = name;
  }

  public boolean printReadable;

  static OutPort outInitial = new OutPort (new LogWriter (new BufferedWriter(new OutputStreamWriter(System.out))), true, true, "<stdout>");

  // It seems better to not pretty-print the error output, since unexpected
  // line break in the middle of an error message may be confusing.
  private static OutPort errInitial = new OutPort (new LogWriter(new OutputStreamWriter(System.err)), false, true, "<stderr>");

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
      java.io.OutputStream strm = URI_utils.getOutputStream(fname);
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
      return new OutPort(wr, fname);
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

  /**
   * Write a character value to a byte-stream.
   * The default transation generates UTF-8 multi-bytes.
   * We support character values above 0xFFFF for future extension.
   */
  public void writeChar (int i)
  {
    write (i);
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

  //public void writeChar(int v);

  public void beginGroup(String typeName, Object type)
  {
    if (objectFormat != null)
      objectFormat.beginGroup(typeName, type, this);
    else
      {
	print('(');
	print(typeName);
      }
  }

  public void endGroup(String typeName)
  {
    if (objectFormat != null)
      objectFormat.endGroup(typeName,this);
    else
      print(')');
  }

  /** Write a attribute for the current group.
   * This is only allowed immediately after a beginGroup. */
  public void beginAttribute(String attrName, Object attrType)
  {
    if (objectFormat != null)
      objectFormat.beginAttribute(attrName, attrType, this);
    else
      {
        print(' ');
        print(attrName);
        print(": ");
      }
  }

  /** No more attributes in this group. */
  public void endAttribute()
  {
    if (objectFormat != null)
      objectFormat.endAttribute(this);
    else
      print(' ');
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
    if (index > 0)
      WriterManager.instance.unregister(index);
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
