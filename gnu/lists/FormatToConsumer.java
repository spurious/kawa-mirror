// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** An object like a Format, but for writing to a Consumer. */

public interface FormatToConsumer
{
  //We should add more methods here.  FIXME.
  //public void writeChar(int v, Consumer out);
  public void writeBoolean(boolean v, Consumer out);

  public void beginGroup(String typeName, Object type, Consumer out);
  public void endGroup(String typeName, Consumer out);

  public void writeObject(Object v, Consumer out);
}
