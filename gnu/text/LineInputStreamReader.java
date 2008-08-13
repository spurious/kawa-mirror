// Copyright (c) 2004, 2007  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import java.io.*;
import gnu.mapping.WrappedException;
/* #ifdef use:java.nio */
import java.nio.*;
import java.nio.charset.*;
/* #endif */

/** A LineBufferedReader that wraps an InputStream.
 * Similar functionality as using an InputStreamReader, but provides hooks
 * to read at the byte level before setting the charset.
 * Optionally uses java.nio.charset directly, for extra flexibility
 * and a possible (but slight and unverified) performance improvement.
 */

public class LineInputStreamReader extends LineBufferedReader
{
  InputStream istrm;
  /* #ifdef use:java.nio */
  byte[] barr = new byte[8192];
  ByteBuffer bbuf;
  char[] carr;
  CharBuffer cbuf = null;

  Charset cset;
  CharsetDecoder decoder;

  public void setCharset (Charset cset)
  {
    this.cset = cset;
    this.decoder = cset.newDecoder();
  }
  /* #endif */

  public void setCharset (String name)
  {
    /* #ifdef use:java.nio */
    Charset cset = Charset.forName(name);
    if (this.cset == null)
      setCharset(cset);
    else if (! cset.equals(this.cset))
      throw new RuntimeException("encoding "+name+" does not match previous "+this.cset);
    /* #else */
    // if (this.in != null)
    //   return; // Should also check for a mismatch
    // try
    //   {
    //     this.in = new InputStreamReader(istrm, name);
    //   }
    // catch (java.io.UnsupportedEncodingException ex)
    //   {
    //     throw new WrappedException(ex);
    //   }
    /* #endif */
  }

  public LineInputStreamReader (InputStream in)
  {
    super((Reader) null);
    /* #ifdef use:java.nio */
    bbuf = ByteBuffer.wrap(barr);
    bbuf.position(barr.length);
    this.istrm = in;
    /* #else */
    // this.istrm = new BufferedInputStream(in);
    /* #endif */
  }

  public void close () throws IOException
  {
    if (in != null)
      in.close();
    istrm.close();
  }

  /* #ifdef use:java.nio */
  private int fillBytes (int remaining) throws java.io.IOException
  {
    int n = istrm.read(barr, remaining, barr.length-remaining);
    bbuf.position(0);
    bbuf.limit(remaining + (n < 0 ? 0 : n));
    return n;
  }
  /* #endif */

  public void markStart () throws java.io.IOException
  {
    /* #ifndef use:java.nio */
    // istrm.mark(200);
    /* #endif */
  }

  public void resetStart (int pos) throws java.io.IOException
  {
    /* #ifdef use:java.nio */
    bbuf.position(pos);
    /* #else */
    // istrm.reset();
    // while (--pos >= 0)
    //   istrm.read();
    /* #endif */
  }

  public int getByte () throws java.io.IOException
  {
    /* #ifdef use:java.nio */
    if (! bbuf.hasRemaining())
      {
        int n = fillBytes(0);
        if (n <= 0)
          return -1;
      }
    return bbuf.get() & 0xFF;
    /* #else */
    // return istrm.read();
    /* #endif */
  }

  public int fill (int len) throws java.io.IOException
  {
    /* #ifdef use:java.nio */
    if (cset == null)
      setCharset("UTF-8");
    if (buffer != carr)
      {
        cbuf = CharBuffer.wrap(buffer);
        carr = buffer;
      }
    cbuf.limit(pos+len);
    cbuf.position(pos);
    boolean eof = false;
    int count;
    for (;;)
      {
        CoderResult cres = decoder.decode(bbuf, cbuf, eof);
        count = cbuf.position() - pos;
        if (count > 0 || ! cres.isUnderflow())
          break;
        int rem = bbuf.remaining();
        if (rem > 0)
          {
            bbuf.compact();
          }
        int n = fillBytes(rem);
        if (n < 0)
          {
            eof = true;
            break;
          }
      }
    return count == 0 && eof ? -1 : count;
    /* #else */
    // if (in == null)
    //   setCharset("UTF-8");
    // return super.fill(len);
    /* #endif */
  }

  /* #ifdef use:java.nio */
  public boolean ready () throws java.io.IOException
  {
    return pos < limit || bbuf.hasRemaining() || istrm.available() > 0;
  }
  /* #endif */
}
