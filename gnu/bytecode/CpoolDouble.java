package codegen;
import java.io.*;

/** A CONSTANT_Double entry in the constant pool. */

public class CpoolDouble extends CpoolEntry
{
  double value;
  private CpoolDouble (ClassType classfile, int h, double val)
  {
    super (classfile, h);
    value = val;
    ++classfile.constant_pool_count;
  }

  final static int hash (double val) { return (int) val; }

  public static CpoolDouble get_const (ClassType classfile, double val)
  {
    int h = hash (val);

    // Check if we already have a matching CONSTANT_Double.
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    if (hash_tab != null) {
      int index = (h & 0x7FFFFFFF) % hash_tab.length;
      CpoolEntry entry;
      for (entry = hash_tab[index]; entry != null; entry = entry.next) {
	if (h == entry.hash
	    && entry instanceof CpoolDouble
	    && ((CpoolDouble)entry).value == val)
	  return (CpoolDouble)entry;
      }
    }
    return new CpoolDouble (classfile, h, val);
  }

  void write (DataOutputStream dstr) throws java.io.IOException {
	dstr.writeByte (6);  // CONSTANT_Double
        dstr.writeDouble (value);
    }
}
