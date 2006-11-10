// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** A Consumer that extends a PrintWriter.  Useful for formatting. */

public class PrintConsumer extends PrintWriter
  implements
  /* #ifdef JAVA5 */
  // Appendable,
  /* #endif */
   Consumer
{
  public PrintConsumer(Consumer out, boolean autoFlush)
  {
    super(out instanceof Writer ? (Writer) out : new ConsumerWriter(out),
	  autoFlush);
  }

  public PrintConsumer(OutputStream out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public PrintConsumer(Writer out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public PrintConsumer(Writer out)
  {
    super(out);
  }

  protected void startNumber()
  {
  }

  /* #ifdef JAVA5 */
  // public PrintConsumer append (char c)
  // {
  //   writeChar(c);
  //   return this;
  // }

  // public PrintConsumer append (CharSequence csq)
  // {
  //   if (csq == null)
  //     csq = "null";
  //   return append(csq, 0, csq.length());
  // }

  // public PrintConsumer append (CharSequence csq, int start, int end)
  // {
  //   if (csq == null)
  //     csq = "null";
  //   for (int i = start; i < end;  i++)
  //     append(csq.charAt(i));
  //   return this;
  // }
  /* #endif */

  public void writeChar(int v)
  {
    print((char) v);
  }

  public void writeBoolean(boolean v)
  {
    print(v);
  }

  public void writeFloat(float v)
  {
    startNumber();
    print(v);
  }

  public void writeDouble(double v)
  {
    startNumber();
    print(v);
  }

  public void writeInt(int v)
  {
    startNumber();
    print(v);
  }

  public void writeLong(long v)
  {
    startNumber();
    print(v);
  }

  public void beginDocument() { }

  public void endDocument() { }


  public void beginGroup(Object type) { }

  public void endGroup() { }

  public void beginAttribute(Object attrType) { }

  public void endAttribute() { }

  public void writeObject(Object v)
  {
    print(v);
  }

  public boolean ignoring()
  {
    return false;
  }

  public void writeChars(String str)
  {
    print(str);
  }
}
