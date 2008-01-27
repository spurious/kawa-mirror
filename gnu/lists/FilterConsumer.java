// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer that wraps some other Consumer. */

public class FilterConsumer
  implements Consumer
{
  protected Consumer base;
  protected boolean skipping;
  /** We seen a startAttribute but not the closing endAttribute. */
  protected boolean inAttribute;
  /** The 'attribute type' from the most recent startAttribute. */
  protected Object attributeType;

  public FilterConsumer (Consumer base)
  {
    this.base = base;
  }

  protected void beforeContent ()
  {
  }

  public void write (int v)
  {
    beforeContent();
    if (! skipping)
      base.write(v);
  }

  public void writeBoolean(boolean v)
  {
    beforeContent();
    if (! skipping)
      base.writeBoolean(v);
  }

  public void writeFloat(float v)
  {
    beforeContent();
    if (! skipping)
      base.writeFloat(v);
  }

  public void writeDouble(double v)
  {
    beforeContent();
    if (! skipping)
      base.writeDouble(v);
  }

  public void writeInt(int v)
  {
    beforeContent();
    if (! skipping)
      base.writeInt(v);
  }

  public void writeLong(long v)
  {
    beforeContent();
    if (! skipping)
      base.writeLong(v);
  }

  public void startDocument()
  {
    if (! skipping)
      base.startDocument();
  }

  public void endDocument()
  {
    if (! skipping)
      base.endDocument();
  }

  public void startElement (Object type)
  {
    if (! skipping)
      base.startElement(type);
  }

  public void endElement ()
  {
    if (! skipping)
      base.endElement();
  }

  public void startAttribute (Object attrType)
  {
    attributeType = attrType;
    inAttribute = true;
    if (! skipping)
      base.startAttribute(attrType);
  }

  public void endAttribute()
  {
    if (! skipping)
      base.endAttribute();
    inAttribute = false;
  }

  public void writeObject(Object v)
  {
    beforeContent();
    if (! skipping)
      base.writeObject(v);
  }

  public boolean ignoring()
  {
    return base.ignoring();
  }

  public void write(char[] buf, int off, int len)
  {
    beforeContent();
    if (! skipping)
      base.write(buf, off, len);
  }

  public void write (String str)
  {
    write(str, 0, str.length());
  }

  /* #ifdef use:java.lang.CharSequence */
  public void write (CharSequence str, int start, int length)
  /* #else */
  // public void write (String str, int start, int length)
  /* #endif */
  {
    beforeContent();
    if (! skipping)
      base.write(str, start, length);
  }

  /* #ifdef JAVA5 */
  public Consumer append (char c)
  {
    write(c);
    return this;
  }

  public Consumer append (CharSequence csq)
  {
    if (csq == null)
      csq = "null";
    append(csq, 0, csq.length());
    return this;
  }

  public Consumer append (CharSequence csq, int start, int end)
  {
    beforeContent();
    if (! skipping)
      {
        if (csq == null)
          csq = "null";
        base.append(csq, start, end);
      }
    return this;
  }
  /* #endif */
}
