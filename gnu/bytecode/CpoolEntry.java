package codegen;
import java.io.*;

/**
 * An entry in the constant pool for a ClassType.
 * Each entry belong to the constant pool table of the "owning"
 * ClassType.  Hashing is used to make each entry unique (with a ClassType).
 * By convention, each sub-class has a static get_const method which is
 * used when a contant pool entry is need:  The get_const method will
 * return an existing matching entry if possible, or allocate a new
 * one if needed.
 * @author	Per Bothner
 */

abstract public class CpoolEntry
{
  /** A hashvalue so we do not get duplicate constant pool entries. */
  int hash;

  /** This entry's index in the constant pool of the owning ClassType. */
  public int index;

  /** The next entry in the same hash bucket
   * (of the owning ClassType's constant_pool_hash). */
  CpoolEntry next;

  public int hashCode () { return hash; }

  abstract void write(DataOutputStream str)
       throws java.io.IOException;

  /**
   * Enter current element into classfile.constant_pool_hash hash table.
   */
  private void add_hashed (ClassType classfile)
  {
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    int index = (hash & 0x7FFFFFFF) % hash_tab.length;
    next = hash_tab[index];
    hash_tab[index] = this;
  }

  public CpoolEntry (ClassType classfile, int h)
  {
     hash = h;
     index = ++classfile.constant_pool_count;

     // (Re-)allocate the classfile.constant_pool array if need be.
     if (classfile.constant_pool == null)
	classfile.constant_pool = new CpoolEntry[60];
     else if (index >= classfile.constant_pool.length) {
       int old_size = classfile.constant_pool.length;
       int new_size = 2 * classfile.constant_pool.length;
       int i;
       CpoolEntry[] new_pool = new CpoolEntry[new_size];
       for (i = 0; i < old_size; i++) {
	 new_pool[i] = classfile.constant_pool[i];
       }
       classfile.constant_pool = new_pool;
     }

     // Re-hash classfile.constant_pool_hash hash_table if needed.
     if (classfile.constant_pool_hash == null)
       classfile.constant_pool_hash = new CpoolEntry[101];
     else {
       int old_size = classfile.constant_pool_hash.length;
       if (index >= 0.60 * old_size) {
	 CpoolEntry[] new_hash = new CpoolEntry[2 * old_size + 1];
	 classfile.constant_pool_hash = new_hash;
	 int i;
	 for (i = 0; i < old_size; i++) {
	   CpoolEntry entry = classfile.constant_pool[i];
	   if (entry != null)
	     entry.add_hashed (classfile);
	 }
       }
     }

     // Enter into classfile.constant_pool array.
     classfile.constant_pool[index] = this;
     // Enter into classfile.constant_pool_hash hash table.
     add_hashed (classfile);
   }
};
