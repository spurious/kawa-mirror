package gnu.bytecode;
import java.io.*;

/** A CONSTANT_Long entry in the constant pool. */

public class CpoolLong extends CpoolEntry
{
  long value;
  private CpoolLong (ClassType classfile, int h, long val)
  {
    super (classfile, h);
    value = val;
    ++classfile.constant_pool_count;
  }

  final static int hash (long val) { return (int) val; }

  public static CpoolLong get_const (ClassType classfile, long val)
  {
    int h = hash (val);

    // Check if we already have a matching CONSTANT_Long.
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    if (hash_tab != null) {
      int index = (h & 0x7FFFFFFF) % hash_tab.length;
      CpoolEntry entry;
      for (entry = hash_tab[index]; entry != null; entry = entry.next) {
	if (h == entry.hash
	    && entry instanceof CpoolLong
	    && ((CpoolLong)entry).value == val)
	  return (CpoolLong)entry;
      }
    }
    return new CpoolLong (classfile, h, val);
  }

  void write (DataOutputStream dstr) throws java.io.IOException {
	dstr.writeByte (5);  // CONSTANT_Long
        dstr.writeLong (value);
    }
}
