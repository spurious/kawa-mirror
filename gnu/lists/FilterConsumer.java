// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer that wraps some other Consumer. */

public class FilterConsumer
  implements Consumer
{
  protected Consumer base;
  protected boolean skipping;

  public FilterConsumer (Consumer base)
  {
    this.base = base;
  }

  public void writeChar(int v)
  {
    if (! skipping)
      base.writeChar(v);
  }

  public void writeBoolean(boolean v)
  {
    if (! skipping)
      base.writeBoolean(v);
  }

  public void writeFloat(float v)
  {
    if (! skipping)
      base.writeFloat(v);
  }

  public void writeDouble(double v)
  {
    if (! skipping)
      base.writeDouble(v);
  }

  public void writeInt(int v)
  {
    if (! skipping)
      base.writeInt(v);
  }

  public void writeLong(long v)
  {
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

  public void beginGroup(String typeName, Object type)
  {
    if (! skipping)
      base.beginGroup(typeName, type);
  }

  public void endGroup(String typeName)
  {
    if (! skipping)
      base.endGroup(typeName);
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    if (! skipping)
      base.beginAttribute(attrName, attrType);
  }

  public void endAttribute()
  {
    if (! skipping)
      base.endAttribute();
  }

  public void writeObject(Object v)
  {
    if (! skipping)
      base.writeObject(v);
  }

  public boolean ignoring()
  {
    return base.ignoring();
  }

  public void writeChars(String str)
  {
    if (! skipping)
      base.writeChars(str);
  }

  public void write(char[] buf, int off, int len)
  {
    if (! skipping)
      base.write(buf, off, len);
  }

  /* #ifdef JAVA5 */
  // public Consumer append (char c)
  // {
  //   if (! skipping)
  //     base.append(c);
  //   return this;
  // }

  // public Consumer append (CharSequence csq)
  // {
  //   if (! skipping)
  //     base.append(csq);
  //   return this;
  // }

  // public Consumer append (CharSequence csq, int start, int end)
  // {
  //   if (! skipping)
  //     base.append(csq, start, end);
  //   return this;
  // }
  /* #endif */
}
