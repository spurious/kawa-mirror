package gnu.bytecode;
import java.io.*;

/** A CONSTANT_NameAndType entry in the constant pool. */

public class CpoolNameAndType extends CpoolEntry {
  CpoolUtf8 name;
  CpoolUtf8 type;
  private CpoolNameAndType (ClassType classfile, int hash,
			    CpoolUtf8 n, CpoolUtf8 t)
  {
    super (classfile, hash);
    name = n;
    type = t;
  }

  final static int hash_of (CpoolUtf8 name, CpoolUtf8 type) {
    return name.hash ^ type.hash;
  }

  public static CpoolNameAndType
  get_const (ClassType classfile, Method method)
  {
    CpoolUtf8 name = CpoolUtf8.get_const (classfile, method.getName());
    CpoolUtf8 type = CpoolUtf8.get_const (classfile, method.getSignature ());
    return get_const (classfile, name, type);
  }

  public static CpoolNameAndType
  get_const (ClassType classfile, Field field)
  {
    CpoolUtf8 name = CpoolUtf8.get_const (classfile, field.getName());
    CpoolUtf8 type = CpoolUtf8.get_const (classfile, field.getSignature ());
    return get_const (classfile, name, type);
  }

  public static CpoolNameAndType
  get_const (ClassType classfile, CpoolUtf8 name, CpoolUtf8 type) {
    int h = hash_of (name, type);

    // Check if we already have a matching CONSTANT_Integer.
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    if (hash_tab != null) {
      int index = (h & 0x7FFFFFFF) % hash_tab.length;
      CpoolEntry entry;
      for (entry = hash_tab[index]; entry != null; entry = entry.next) {
	if (h == entry.hash
	    && entry instanceof CpoolNameAndType
	    && ((CpoolNameAndType)entry).name == name
	    && ((CpoolNameAndType)entry).type == type)
	  return (CpoolNameAndType)entry;
      }
    }
    return new CpoolNameAndType (classfile, h, name, type);
  }

  void write (DataOutputStream dstr) throws java.io.IOException {
    dstr.writeByte (12);  // CONSTANT_NameAndType
    dstr.writeShort (name.index);
    dstr.writeShort (type.index);
  }
}
