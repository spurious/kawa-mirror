package codegen;
import java.io.*;

public class
CpoolUtf8 extends CpoolEntry
{
  byte[] str;

  private CpoolUtf8 (ClassType classfile, int h, byte[] s)
    {
      super (classfile, h);
      str = s;
    }

  public static CpoolUtf8 get_const (ClassType classfile, String s) {
    return CpoolUtf8.get_const (classfile, ClassType.to_utf8 (s));
  }

  public static CpoolUtf8 get_const (ClassType classfile, byte[] s)
    {
      int h = hash_of (s);

      // Check if we already have a matching CONSTANT_Utf8.
      CpoolEntry[] hash_tab = classfile.constant_pool_hash;
      if (hash_tab != null)
	{
	  int index = (h & 0x7FFFFFFF) % hash_tab.length;
	  CpoolEntry entry;
	  for (entry = hash_tab[index]; entry != null; entry = entry.next)
	    {
	      if (h == entry.hash
		  && entry instanceof CpoolUtf8)
		{
		  CpoolUtf8 utf = (CpoolUtf8)entry;
		  if (Scope.equals (utf.str, s))
		    return utf;
		}
	    }
	}
      return new CpoolUtf8 (classfile, h, s);
    }

  static int hash_of (byte[] str)
    {
      int len = str.length;
      int h = 0;
      for (int i = len; --i >= 0; )
	h = (h * 37) + str[i];
      return h;
    }

  void write (DataOutputStream dstr) throws java.io.IOException {
	dstr.writeByte (1);  // CONSTANT_Utf8
	dstr.writeShort (str.length);
        dstr.write (str);
    }
 
};
