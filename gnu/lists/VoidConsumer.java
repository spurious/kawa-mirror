// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer that does nothing. */

public class VoidConsumer implements Consumer
{
  public static VoidConsumer instance = new VoidConsumer();

  public static VoidConsumer getInstance() { return instance; }

  public void writeChar(int v) { }
  public void writeBoolean(boolean v) { }

  public void writeFloat(float v) { }
  public void writeDouble(double v) { }
  public void writeInt(int v) { }
  public void writeLong(long v) { }

  public void beginGroup(String typeName, Object type) { }
  public void endGroup(String typeName) { }

  public void beginAttribute(String attrName, Object attrType) { }

  public void endAttributes() { }

  public void writeObject(Object v) { }

  /** True if consumer is ignoring rest of group.
   * The producer can use this information to skip ahead. */
  public boolean ignoring()
  {
    return true;
  }

  public void writeChars(String str) { }
  public void write(char[] buf, int off, int len) { }
}
