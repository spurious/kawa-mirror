package kawa.lang;
import java.io.*;

/**
 * An extended Writer.
 */

public class OutPort extends PrintWriter implements Printable
{
  String name;
  private Writer base;

  public OutPort (OutputStream out)
  {
    this (out, null);
  }

  public OutPort (OutputStream out, String name)
  {
    this(new BufferedWriter(new OutputStreamWriter(out)), true, name);
  }

  public OutPort (Writer out)
  {
    super (out);
    base = base;
  }

  public OutPort (Writer out, String name)
  {
    super (out);
    this.name = name;
    base = out;
  }

  public OutPort (Writer out, boolean autoflush, String name)
  {
    super (out, autoflush);
    this.name = name;
    base = out;
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
}
