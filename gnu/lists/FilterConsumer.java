// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer that wraps some other Consumer. */

public class FilterConsumer
  implements Consumer
{
  protected Consumer base;
  protected boolean skipping;
  /** We seen a beginAttribute but not the closing endAttribute. */
  protected boolean inAttribute;
  /** The 'attribute type' from the most recent beginAttribute. */
  protected Object attributeType;

  public FilterConsumer (Consumer base)
  {
    this.base = base;
  }

  protected void beforeContent ()
  {
  }

  public Consumer append (char c)
  {
    beforeContent();
    if (! skipping)
      base.append(c);
    return this;
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

  public void beginDocument()
  {
    if (! skipping)
      base.beginDocument();
  }

  public void endDocument()
  {
    if (! skipping)
      base.endDocument();
  }

  public void beginGroup (Object type)
  {
    if (! skipping)
      base.beginGroup(type);
  }

  public void endGroup ()
  {
    if (! skipping)
      base.endGroup();
  }

  public void beginAttribute (Object attrType)
  {
    attributeType = attrType;
    inAttribute = true;
    if (! skipping)
      base.beginAttribute(attrType);
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

  public void writeChars(String str)
  {
    beforeContent();
    if (! skipping)
      base.writeChars(str);
  }

  public void write(char[] buf, int off, int len)
  {
    beforeContent();
    if (! skipping)
      base.write(buf, off, len);
  }

  /* #ifdef use:java.lang.CharSequence */
  public Consumer append (CharSequence csq)
  {
    beforeContent();
    if (! skipping)
      base.append(csq);
    return this;
  }

  public Consumer append (CharSequence csq, int start, int end)
  {
    beforeContent();
    if (! skipping)
      base.append(csq, start, end);
    return this;
  }
  /* #else */
  // public Consumer append (String str)
  // {
  //   beforeContent();
  //   if (! skipping)
  //     base.append(str);
  //   return this;
  // }
  /* #endif */
}
