// Copyright (c) 1997  Cygnus Solutions, Inc.
// This is free software;  for terms and warranty disclaimer see ./LICENSE.

package gnu.bytecode;
import java.io.*;

public class CpoolUtf8 extends CpoolEntry
{
  String string;

  CpoolUtf8 () { }

  CpoolUtf8 (ConstantPool cpool, int h, String s)
  {
    super (cpool, h);
    string = s;
  }

  public int getTag() { return 1; } // CONSTANT_CUtf8

  void write (DataOutputStream dstr) throws java.io.IOException
  {
	dstr.writeByte (1);  // CONSTANT_Utf8
	dstr.writeUTF (string);
  }

  public void print (ClassTypeWriter dst, int verbosity)
  {
    if (verbosity > 0)
      dst.print("Utf8: ");
    dst.print('\"');
    dst.print(string);
    dst.print('\"');
  }
};
