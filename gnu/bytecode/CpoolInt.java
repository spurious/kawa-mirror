package gnu.bytecode;
import java.io.*;

/** A CONSTANT_Integer entry in the constant pool. */

public class CpoolInt extends CpoolEntry
{
  int value;
  private CpoolInt (ClassType classfile, int h, int val) {
    super (classfile, h);
    value = val;
  }

  final static int hash (int val) { return val; }

  public static CpoolInt get_const (ClassType classfile, int val) {
    int h = hash (val);

    // Check if we already have a matching CONSTANT_Integer.
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    if (hash_tab != null) {
      int index = (h & 0x7FFFFFFF) % hash_tab.length;
      CpoolEntry entry;
      for (entry = hash_tab[index]; entry != null; entry = entry.next) {
	if (h == entry.hash
	    && entry instanceof CpoolInt
	    && ((CpoolInt)entry).value == val)
	  return (CpoolInt)entry;
      }
    }
    return new CpoolInt (classfile, h, val);
  }

  void write (DataOutputStream dstr) throws java.io.IOException {
	dstr.writeByte (3);  // CONSTANT_Integer
        dstr.writeInt (value);
    }
}
