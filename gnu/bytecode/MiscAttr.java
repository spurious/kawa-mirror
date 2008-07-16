// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/* Represents a generic uninterpreted or unknown attribute.
 * @author      Per Bothner
 */

public class MiscAttr extends Attribute
{
  byte[] data;
  int offset;
  int length;

  public MiscAttr(String name, byte[] data, int offset, int length)
  {
    super(name);
    this.data = data;
    this.offset = offset;
    this.length = length;
  }
  
  public MiscAttr(String name, byte[] data)
  {
    this(name, data, 0, data.length);
  }
  
  public int getLength() { return length; }

  public void write (DataOutputStream dstr)
    throws java.io.IOException
  {
    dstr.write(data, offset, length);
  }

  public void print (ClassTypeWriter dst)
  {
    super.print(dst);
    int len = getLength();
    for (int i = 0;  i < len;  )
      {
        int b = data[i];
        if ((i % 20) == 0)
          dst.print(' ');
        dst.print(' ');
        dst.print(Character.forDigit((b >> 4) & 15, 16));
        dst.print(Character.forDigit(b & 15, 16));
        i++;
        if ((i % 20) == 0 || i == len)
          dst.println();
      }
  }

}

