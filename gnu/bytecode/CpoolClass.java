package gnu.bytecode;
import java.io.*;

/** A CONSTANT_Class entry in the constant pool. */

public class CpoolClass extends CpoolEntry {
  CpoolUtf8 name;
  private CpoolClass (ClassType classfile, int hash, CpoolUtf8 n) {
    super (classfile, hash);
    name = n;
  }

  final static int hash_of (CpoolUtf8 name) { return name.hash ^ 0xF0F; }

  public static CpoolClass get_const (ClassType classfile, String name)
  {
    return get_const (classfile, CpoolUtf8.get_const (classfile,
						      name.replace ('.', '/')));
  }

  public static CpoolClass get_const (ClassType classfile, CpoolUtf8 name)
  {
    int h = hash_of (name);

    // Check if we already have a matching CONSTANT_Class.
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    if (hash_tab != null) {
      int index = (h & 0x7FFFFFFF) % hash_tab.length;
      CpoolEntry entry;
      for (entry = hash_tab[index]; entry != null; entry = entry.next) {
	if (h == entry.hash
	    && entry instanceof CpoolClass)
	  {
	    CpoolClass ent = (CpoolClass) entry;
	    if (ent.name == name)
	      return ent;
	  }
      }
    }
    return new CpoolClass (classfile, h, name);
  }

  void write (DataOutputStream dstr) throws java.io.IOException {
    dstr.writeByte (7);  // CONSTANT_Class
    dstr.writeShort (name.index);
  }
}
