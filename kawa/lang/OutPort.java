package kawa.lang;
import java.io.*;

/**
 * An OutputStream that handles characters (rather than just bytes).
 */

public class OutPort extends PrintStream
{
  public OutPort (OutputStream out)
  {
    super (out);
  }

  public boolean printReadable;

  // For now, these are static.  They should probably be thread-local.
  private static OutPort out = new OutPort (System.out);
  private static OutPort err = new OutPort (System.err);

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

  /**
   * Write a character value to a byte-stream.
   * The default transation generates UTF-8 multi-bytes.
   * We support character values above 0xFFFF for future extension.
   */
  public void writeChar (int i)
  {
    if (i < 0x7F)
      write (i);
    else if (i <= 0x7FF)  // 11 bits
      {
	write (0xC0 | ((i >>  6) & 0x1F));
	write (0x80 | ((i >>  0) & 0x3F));
      }
    else if (i <= 0xFFFF) // 16 bits
      {
	write (0xE0 | ((i >> 12) & 0x0F));
	write (0x80 | ((i >>  6) & 0x3F));
	write (0x80 | ((i >>  0) & 0x3F));
      }
    else if (i <= 0x1FFFFF) // 21 bits
      {
	write (0xF0 | ((i >> 18) & 0x07));
	write (0x80 | ((i >> 12) & 0x3F));
	write (0x80 | ((i >>  6) & 0x3F));
	write (0x80 | ((i >>  0) & 0x3F));
      }
    else if (i <= 0x3FFFFFF) // 26 bits
      {
	write (0xF8 | ((i >> 24) & 0x03));
	write (0x80 | ((i >> 18) & 0x3F));
	write (0x80 | ((i >> 12) & 0x3F));
	write (0x80 | ((i >>  6) & 0x3F));
	write (0x80 | ((i >>  0) & 0x3F));
      }
    else // 31 bits
      {
	write (0xFC | ((i >> 30) & 0x01));
	write (0x80 | ((i >> 24) & 0x3F));
	write (0x80 | ((i >> 18) & 0x3F));
	write (0x80 | ((i >> 12) & 0x3F));
	write (0x80 | ((i >>  6) & 0x3F));
	write (0x80 | ((i >>  0) & 0x3F));
      }
  }

  public void print (char c)
  {
    writeChar ((int)c);
  }

  public void print (char[] s)
  {
    int n = s.length;
    for (int i = 0; i < n; i++)
      writeChar ((int) s[i]);
  }

  public void print (String s)
  {
    int n = s.length();
    for (int i = 0 ; i < n ; i++)
      writeChar ((int) s.charAt(i));
  }

  public void writeSchemeObject (Object obj, boolean readable)
  {
    boolean saveReadable = printReadable;
    try
      {
	printReadable = readable;
	kawa.lang.print.print (obj, this);
      }
    finally
      {
	printReadable = saveReadable;
      }
  }
}
