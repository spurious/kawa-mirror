// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** A Consumer that extends a PrintWriter.  Useful for formatting. */

public class PrintConsumer extends PrintWriter implements Consumer
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
    print(v);
  }

  public void writeLong(long v)
  {
    startNumber();
    print(v);
  }

  public void beginDocument() { }

  public void endDocument() { }


  public void beginGroup(String typeName, Object type) { }

  public void endGroup(String typeName) { }

  public void beginAttribute(String attrName, Object attrType) { }

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
