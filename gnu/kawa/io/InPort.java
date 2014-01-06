package gnu.kawa.io;
import java.io.*;
import gnu.mapping.Environment;
import gnu.mapping.ThreadLocation;
import gnu.text.*;
import gnu.lists.Consumer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;

public class InPort extends LineBufferedReader implements Printable
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

    public static InPort openFile(Object fname) throws java.io.IOException {
        Path path = Path.valueOf(fname);
        return openFile(path.openInputStream(), path);
    }

    public static InPort openFile(Object fname, Object conv)
        throws java.io.IOException {
        Path path = Path.valueOf(fname);
        return openFile(path.openInputStream(), path, conv);
    }

    public static InPort openFile(InputStream strm, Path path)
            throws java.io.UnsupportedEncodingException {
        Object conv = Environment.user().get("port-char-encoding");
        return openFile(strm, path, conv);
    }

    public static InPort openFile(InputStream strm, Path path, Object conv)
            throws java.io.UnsupportedEncodingException {
        if (conv == Boolean.FALSE) {
            return new BinaryInPort(strm, path);
        }
        else if (! (strm instanceof BufferedInputStream))
            strm = new BufferedInputStream(strm);
        Reader rdr;
        if (conv instanceof Charset)
            rdr = new InputStreamReader(strm, (Charset) conv);
        else if (conv instanceof CharsetDecoder)
            rdr = new InputStreamReader(strm, (CharsetDecoder) conv);
        else if (conv != null && conv != Boolean.TRUE) {
            String enc = conv.toString();
            try {
                rdr = new InputStreamReader(strm, enc);
            } catch (UnsupportedEncodingException ex) {
                throw new RuntimeException("unknown character encoding: "+enc);
            }
        } else
            rdr = new InputStreamReader(strm);
        InPort port = new InPort(rdr, path);
        port.setConvertCR(true);
        return port;
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
