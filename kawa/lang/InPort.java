package kawa.lang;
import gnu.math.*;
import java.io.*;

public class InPort extends LineBufferedReader implements Printable
{
  String name;

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public InPort (Reader in)
  {
    super (in);
  }

  public InPort (Reader in, String name)
  {
    this (in);
    this.name = name;
  }

  public InPort (InputStream in)
  {
    super (in);
  }

  public InPort (InputStream in, String name)
  {
    this (in);
    this.name = name;
  }

  public static Reader convertToReader (InputStream in, Object conv)
  {
    if (conv != null && conv != Boolean.TRUE)
      {
	String enc = (conv == Boolean.FALSE ? "8859_1" : conv.toString());
	try
	  {
	    return new java.io.InputStreamReader(in, enc);
	  }
	catch (java.io.UnsupportedEncodingException ex)
	  {
	    throw new RuntimeException("unknown character encoding: "+enc);
	  }
      }
    return new java.io.InputStreamReader(in);
  }

  public InPort (InputStream in, String name, Object conv)
    throws java.io.UnsupportedEncodingException
  {
    this (convertToReader(in, conv), name);
    if (conv == Boolean.FALSE)
      {
	// Use a fixed-size buffer.  This prevents really-long "lines"
	// from causing the buffer to grow to accomodate them.
	try
	  {
	    setBuffer(new char[2048]);
	  }
	catch (java.io.IOException ex) { /* ignored */ }
      }
    else
      setConvertCR(true);
  }

  private static InPort systemInPort = new SysInPort (System.in, "<stdin>",
						      OutPort.outDefault());
  private static InPort defaultInPort = systemInPort;

  static public InPort inDefault ()
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      return ((Future) thread).in;
    else
      return defaultInPort;
  }

  static public void setInDefault (InPort in)
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      ((Future) thread).in = in;
    else
      defaultInPort = in;
  }

  public static InPort openFile(String fname)
    throws java.io.UnsupportedEncodingException,
           java.io.FileNotFoundException
  {
    java.io.InputStream strm = new java.io.FileInputStream(fname);
    strm = new java.io.BufferedInputStream(strm);
    return openFile(strm, fname);
  }

  public static InPort openFile(InputStream strm, String fname)
    throws java.io.UnsupportedEncodingException
  {
    return new InPort(strm, fname,
		      Environment.user().get("port-char-encoding"));
  }

  char readState = '\n';
  /** Return a character that indicates what we are currently reading.
    * Returns '\n' if we are not inside read; '\"' if reading a string;
    * '|' if inside a comment; '(' if inside a list; and
    * ' ' if otherwise inside a read. */
  public char getReadState () { return readState; }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<input-port");
    if (name != null)
      {
	ps.print (' ');
	ps.print (name);
      }
    ps.print ('>');
  }
}
