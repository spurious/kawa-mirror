package gnu.bytecode;
import java.io.*;

public class
CpoolUtf8 extends CpoolEntry
{
  String string;

  private CpoolUtf8 (ClassType classfile, int h, String s)
    {
      super (classfile, h);
      string = s;
    }

  public static CpoolUtf8 get_const (ClassType classfile, String s)
    {
      s = s.intern();
      int h = s.hashCode();

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
		  if (utf.string == s)
		    return utf;
		}
	    }
	}
      return new CpoolUtf8 (classfile, h, s);
    }

  void write (DataOutputStream dstr) throws java.io.IOException {
	dstr.writeByte (1);  // CONSTANT_Utf8
	dstr.writeUTF (string);
    }
 
};
