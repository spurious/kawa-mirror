package gnu.mapping;
import java.io.*;
import java.text.*;
import gnu.text.*;
import gnu.lists.*;

/**
 * An extended PrintWriter.
 */

public class OutPort extends PrintConsumer implements Printable
{
  String name;
  private Writer base;

  // To keep track of column-numbers, we use a helper class.
  // Otherwise, it is too painful, as there is no documented
  // interface that would allow PrintWriter to be cleanly extended ...
  // The helper class also lets us make transparent use of WriterManager.
  PrettyWriter bout;

  /** An index into the WriterManager's internal table. */
  protected int index;
  
  OutPort(Writer base, PrettyWriter out, boolean autoflush)
  {
    super(out, autoflush);
    this.bout = out;
    this.base = base;
    index = WriterManager.instance.register(out);
  }

  public OutPort(Writer base, boolean printPretty, boolean autoflush)
  {
    this(base, new PrettyWriter(base, printPretty), autoflush);
  }

  public OutPort(Writer base, boolean printPretty,
		 boolean autoflush, String name)
  {
    this(base, new PrettyWriter(base, printPretty), autoflush);
    this.name = name;
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
    this(out, false, false);
  }

  public OutPort (Writer base, String name)
  {
    this(base, false, false);
    this.name = name;
  }

  public OutPort (Writer base, boolean autoflush, String name)
  {
    this (base, false, autoflush);
    this.name = name;
  }

  public boolean printReadable;

  // For now, these are static.  They should probably be thread-local.
  private static OutPort outInitial = new OutPort (new LogWriter (new BufferedWriter(new OutputStreamWriter(System.out))), true, true, "<stdout>");
  private static OutPort out = outInitial;

  private static OutPort errInitial = new OutPort (new LogWriter(new OutputStreamWriter(System.err)), true, true, "<stderr>");
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

  protected static final int WORD = -2;
  protected int prev = '\n';

  protected static final boolean isWordChar(char ch)
  {
    return Character.isJavaIdentifierPart(ch) || ch == '-' || ch == '+';
  }

  private void startWord()
  {
    /*
    if (prev == WORD || isWordChar((char) prev))
      {
	super.write(' ');
      }
    */
    prev = WORD;
  }

  public void write (int c)
  {
    // if (prev == WORD && isWordChar((char) c))    out.print(' ');
    super.write(c);
    prev = c;
  }

  public void write (char[] buffer, int start, int count)
  {
    if (count > 0)
      {
	// if (prev == WORD && isWordChar(buffer[start]))  out.print(' ');
	super.write(buffer, start, count);
	prev = buffer[start+count-1];
      }
  }

  public void write(String v)
  {
    int len = v.length();
    if (len == 0)
      return;
    //if (prev == WORD && isWordChar(v.charAt(0)))  out.write(' ');
    prev = v.charAt(len-1);
    super.write(v);
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

  public FormatToConsumer objectFormat;

  public void print(char v)
  {
    /*
    if (prev == WORD && isWordChar((char) v))
      {
	out.print(' ');
      }
    */
    super.print(v);
    prev = v;
  }

  public void print(int v)
  {
    startWord();
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format((long) v));
  }

  public void print(long v)
  {
    startWord();
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format(v));
  }

  public void print(double v)
  {
    startWord();
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format(v));
  }

  public void print(float v)
  {
    startWord();
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
    startWord();
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
      out.print(')');
    prev = ')';
  }

  /** Write a attribute for the current group.
   * This is only allowed immediately after a beginGroup. */
  public void beginAttribute(String attrName, Object attrType)
  {
    print(' ');
    print(attrName);
    print(": ");
    prev = WORD;
  }

  /** No more attributes in this group. */
  public void endAttribute()
  {
    prev = WORD;
    print(' ');  // FIXME
  }

  public void writeChars(String str)
  {
    print(str);
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
    super.close();
    WriterManager.instance.unregister(index);
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

  public void writeBreakLinear()
  {
    writeBreak(PrettyWriter.NEWLINE_LINEAR);
  }

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
