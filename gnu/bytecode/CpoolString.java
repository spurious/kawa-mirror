package codegen;
import java.io.*;

/** A CONSTANT_String entry in the constant pool. */

public class CpoolString extends CpoolEntry
{
  CpoolUtf8 str;

  private CpoolString (ClassType classfile, int hash, CpoolUtf8 str)
  {
    super (classfile, hash);
    this.str = str;
  }

  final static int hash_of (CpoolUtf8 str) { return str.hash ^ 0xF30F; }

  public static CpoolString get_const (ClassType classfile, CpoolUtf8 str)
  {
    int h = hash_of (str);

    // Check if we already have a matching CONSTANT_String.
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    if (hash_tab != null) {
      int index = (h & 0x7FFFFFFF) % hash_tab.length;
      CpoolEntry entry;
      for (entry = hash_tab[index]; entry != null; entry = entry.next) {
	if (h == entry.hash && entry instanceof CpoolString)
	  {
	    CpoolString ent = (CpoolString) entry;
	    if (ent.str == str)
	      return ent;
	  }
      }
    }
    return new CpoolString (classfile, h, str);
  }

  void write (DataOutputStream dstr) throws java.io.IOException
  {
    dstr.writeByte (8);  // CONSTANT_String
    dstr.writeShort (str.index);
  }
}

