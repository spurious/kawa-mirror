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
  private static OutPort outInitial = new OutPort (new LogWriter (new BufferedWriter(new OutputStreamWriter(System.err))), true, "<stdout>");
  private static OutPort out = outInitial;

  private static OutPort errInitial = new OutPort (new LogWriter(new OutputStreamWriter(System.err)), "<stderr>");
  private static OutPort err = errInitial;

  static public OutPort outDefault ()
  {
    return out;
  }

  static public void setOutDefault (OutPort o)
  {
    out = o;
  }

  static public OutPort errDefault ()
  {
    return err;
  }

  static public void setErrDefault (OutPort e)
  {
    err = e;
  }

  public void echo (char[] buf, int off, int len)  throws java.io.IOException
  {
    if (base instanceof LogWriter)
      ((LogWriter)base).echo(buf, off, len);
  }

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
