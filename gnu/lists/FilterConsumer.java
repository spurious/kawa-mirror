// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

public class FilterConsumer implements Consumer
{
  protected Consumer base;

  public FilterConsumer (Consumer base)
  {
    this.base = base;
  }

  public void writeChar(int v)
  {
    base.writeChar(v);
  }

  public void writeBoolean(boolean v)
  {
    base.writeBoolean(v);
  }

  public void writeFloat(float v)
  {
    base.writeFloat(v);
  }

  public void writeDouble(double v)
  {
    base.writeDouble(v);
  }

  public void writeInt(int v)
  {
    base.writeInt(v);
  }

  public void writeLong(long v)
  {
    base.writeLong(v);
  }

  public void beginGroup(String typeName, Object type)
  {
    base.beginGroup(typeName, type);
  }

  public void endGroup(String typeName)
  {
    base.endGroup(typeName);
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    base.beginAttribute(attrName, attrType);
  }

  public void endAttributes()
  {
    base.endAttributes();
  }

  public void writeObject(Object v)
  {
    base.writeObject(v);
  }

  public boolean ignoring()
  {
    return base.ignoring();
  }

  public void writeChars(String str)
  {
    base.writeChars(str);
  }

  public void write(char[] buf, int off, int len)
  {
    base.write(buf, off, len);
  }
}
