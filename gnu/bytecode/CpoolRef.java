package gnu.bytecode;
import java.io.*;

/** A CONSTANT_{Field,Method,InterfaceMethod}Ref entry in the constant pool. */

public class
CpoolRef extends CpoolEntry
{
  CpoolClass clas;
  CpoolNameAndType nameAndType;

  /**
   * The specific kind of Ref constant:
   * CONSTANT_Fieldref (9), CONSTANT_Methodref (10), or
   * CONSTANT_InterfaceMethodref (11).
   */
  int tag;

  private CpoolRef (ClassType classfile, int hash, int tag,
		    CpoolClass clas, CpoolNameAndType nameAndType)
  {
    super (classfile, hash);
    this.tag = tag;
    this.clas = clas;
    this.nameAndType = nameAndType;
  }

  final static int hash_of (CpoolClass clas, CpoolNameAndType nameAndType)
  {
    return clas.hash ^ nameAndType.hash;
  }

  public static CpoolRef
  get_const (ClassType classfile, int tag,
	     CpoolClass clas, CpoolNameAndType nameAndType)
  {
    int h = hash_of (clas, nameAndType);

    // Check if we already have a matching CONSTANT_Integer.
    CpoolEntry[] hash_tab = classfile.constant_pool_hash;
    if (hash_tab != null) {
      int index = (h & 0x7FFFFFFF) % hash_tab.length;
      CpoolEntry entry;
      for (entry = hash_tab[index]; entry != null; entry = entry.next) {
	if (h == entry.hash
	    && entry instanceof CpoolRef)
	  {
	    CpoolRef ref = (CpoolRef) entry;
	    if (ref.tag == tag
		&& ref.clas == clas
		&& ref.nameAndType== nameAndType)
	      return ref;
	  }
      }
    }
    return new CpoolRef (classfile, h, tag, clas, nameAndType);
  }

  public static CpoolRef
  get_const (ClassType classfile, Method method)
  {
    CpoolClass clas
      = CpoolClass.get_const (classfile, method.classfile.this_name);
    int tag;
    if ((method.getDeclaringClass().access_flags & Access.INTERFACE) == 0)
      tag = 10; // CONSTANT_Methodref
    else
      tag = 11; // CONSTANT_InterfaceMethodref
    CpoolNameAndType nameType = CpoolNameAndType.get_const (classfile, method);
    return get_const (classfile, tag, clas, nameType);
  }

  public static CpoolRef
  get_const (ClassType classfile, Field field)
  {
    CpoolClass clas
      = CpoolClass.get_const (classfile, field.owner.this_name);
    int tag = 9;  // CONSTANT_Fieldref
    CpoolNameAndType nameType = CpoolNameAndType.get_const (classfile, field);
    return get_const (classfile, tag, clas, nameType);
  }

  void write (DataOutputStream dstr) throws java.io.IOException
  {
    dstr.writeByte (tag);
    dstr.writeShort (clas.index);
    dstr.writeShort (nameAndType.index);
  }

}

