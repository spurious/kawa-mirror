// Copyright (c) 1997  Cygnus Solutions, Inc.
// This is free software;  for terms and warranty disclaimer see ./LICENSE.

package gnu.bytecode;
import java.io.*;

/** A CONSTANT_String entry in the constant pool. */

public class CpoolString extends CpoolEntry
{
  CpoolUtf8 str;

  CpoolString() { }

  CpoolString (ConstantPool cpool, int hash, CpoolUtf8 str)
  {
    super (cpool, hash);
    this.str = str;
  }

  public int getTag() { return ConstantPool.STRING; }

  final static int hashCode (CpoolUtf8 str) { return str.hash ^ 0xF30F; }

  void write (DataOutputStream dstr) throws java.io.IOException
  {
    dstr.writeByte (ConstantPool.STRING);
    dstr.writeShort (str.index);
  }

  public void print (ClassTypeWriter dst, int verbosity)
  {
    if (verbosity > 0)
      {
	dst.print("String ");
	if (verbosity == 2)
	  dst.printOptionalIndex(str);
      }
    dst.printConstantTersely(str.index, 1);
  }
}
