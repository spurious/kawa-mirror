// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.util;
import java.io.*;

/** A constant AbstractString implemented as a java.lang.String wrapper. */

public class ConstantString extends AbstractString implements Externalizable
{
  String value;

  public ConstantString ()
  {
  }

  public ConstantString(String value)
  {
    this.value = value;
  }

  public final int length ()
  {
    return value.length();
  }

  public String toString ()
  {
    return value;
  }

  public String substring(int start, int end)
  {
    return value.substring(start, end);
  }

  public AbstractString subString(int fromPosition, int toPosition)
  {
    return new ConstantString(substring(fromPosition, toPosition));
  }

  public char charAt (int index)
  {
    return value.charAt(index);
  }

  public void setCharAt (int index, char ch)
  {
    throw new RuntimeException("trying to modify constant string");
  }

  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    value.getChars(srcBegin, srcEnd, dst, dstBegin);
  }

  public void writeTo(int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    dest.write(value, start, count);
  }

  public void writeTo(java.io.Writer dest) throws java.io.IOException
  {
    dest.write(value);
  }

  public FString copy ()
  {
    return new FString(value);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(value);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    value = (String) in.readObject();
  }
}
