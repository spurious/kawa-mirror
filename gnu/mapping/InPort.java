package gnu.mapping;
import java.io.*;
import gnu.text.*;
import gnu.lists.Consumer;

public class InPort extends gnu.text.LineBufferedReader implements Printable
{
  public InPort (Reader in)
  {
    super(in);
  }

  public InPort (Reader in, Path path)
  {
    this(in);
    setPath(path);
  }

  public InPort (InputStream in)
  {
    super(in);
  }

  public InPort (InputStream in, Path path)
  {
    this(in);
    setPath(path);
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

  public InPort (InputStream in, Path path, Object conv)
    throws java.io.UnsupportedEncodingException
  {
    this (convertToReader(in, conv), path);
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

  private static InPort systemInPort
  = new TtyInPort (System.in, Path.valueOf("/dev/stdin"), OutPort.outInitial);
  public static final ThreadLocation inLocation
    = new ThreadLocation("in-default");
  static { inLocation.setGlobal(systemInPort); }

  static public InPort inDefault ()
  {
    return (InPort) inLocation.get();
  }

  static public void setInDefault (InPort in)
  {
    inLocation.set(in);
  }

  public static InPort openFile(Object fname)
    throws java.io.IOException
  {
    Path path = Path.valueOf(fname);
    java.io.InputStream strm = path.openInputStream();
    strm = new java.io.BufferedInputStream(strm);
    return openFile(strm, path);
  }

  public static InPort openFile(InputStream strm, Object fname)
    throws java.io.UnsupportedEncodingException
  {
    return new InPort(strm, Path.valueOf(fname),
		      Environment.user().get("port-char-encoding"));
  }

   public void print (Consumer out)
  {
    out.write("#<input-port");
    String name = getName();
    if (name != null)
      {
	out.write(' ');
	out.write(name);
      }
    out.write('>');
  }
}
