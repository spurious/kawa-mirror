package gnu.bytecode;
import java.io.*;

public class ClassType extends Type {
  public static final int minor_version = 3;
  public static final int major_version = 45;

  int this_class;
  int super_class; // constant pool index of super class, or -1 if unknown
  int[] interfacesImplemented;
  public int access_flags;

  /** The index of the SourceFile attribute (if > 0). */ 
  int sourcefile_index;

  boolean emitDebugInfo = true;

  // Constant-pool-related fields. */

  /** The entries in the constant pool.
   * The first element (constant_pool[0]) is an unused dummy. */
  CpoolEntry[] constant_pool;

  /** Number of elements in the constant pool, not counting
   * the initial dummy element (with index 0). */
  int constant_pool_count;

  CpoolEntry[] constant_pool_hash;

  /** Get index in constant pool of a CONSTANT_Utf8 constant,
    * Re-use existing constant if there is one;  otherwise allocate new entry.
   * @param value of constant (as Unicode String)
   */
  public int get_utf8_const (String str) {
    return CpoolUtf8.get_const (this, str).index;
  }

  public int get_class_const (ClassType ctype)
  {
    return CpoolClass.get_const (this, ctype.this_name).index;
  }

  public int get_class_const (CpoolUtf8 name)
  {
    return CpoolClass.get_const (this, name).index;
  }
  public int get_class_const (int name_index) {
    return get_class_const ((CpoolUtf8)constant_pool[name_index]);
  }
  public int get_int_const (int i) {
    return CpoolInt.get_const (this, i).index;
  }

  /**
   * Sets the name of the class being defined in this classfile.
   * @param name the name to give to the class
   * @return the the constant pool entry for the class
   */
  CpoolClass setName (String name)
  {
    this_name = name;
    name = name.replace ('.', '/');
    setSignature("L"+name+";");
    CpoolUtf8 name_entry = CpoolUtf8.get_const (this, name);
    CpoolClass class_entry = CpoolClass.get_const (this, name_entry);
    this_class = class_entry.index;
    return class_entry;
  }

  /** Set the name of the SourceFile associated with this class. */
  public void setSourceFile (String name)
  {
    sourcefile_index = CpoolUtf8.get_const (this, name).index;
  }

  /**
   * Set the superclass of the is class.
   * param name name of super class, or null if this is "Object".
   */
  public CpoolClass setSuper (String name) {
    if (name == null) {
      super_class = 0;
      return null;
    }
    else {
      name = name.replace ('.', '/');
      CpoolUtf8 name_entry = CpoolUtf8.get_const (this, name);
      CpoolClass class_entry = CpoolClass.get_const (this, name_entry);
      super_class = class_entry.index;
      return class_entry;
    }
  }

  public CpoolClass setSuper (ClassType superClass)
  {
    return setSuper (superClass.this_name);
  }

  public void setInterfaces (ClassType[] interfaces)
  { int n = interfaces.length;
    interfacesImplemented = new int [n];
    for (int i = 0;  i < n;  i++)
      {
	String name = interfaces[i].this_name.replace ('.', '/');
	CpoolUtf8 name_entry = CpoolUtf8.get_const (this, name);
	CpoolClass class_entry = CpoolClass.get_const (this, name_entry);
	interfacesImplemented[i] = class_entry.index;
      }
  }

  public ClassType () {
    super_class = -1;
  }
  public ClassType (String class_name)
  {
    super();
    size = 4;
    setName(class_name);

    super_class = -1;
  }

  Field fields;
  int fields_count;
  Field last_field;
  /**  Constant pool index of "ConstantValue". */
  int ConstantValue_name_index;

  /** Constant pool index of "Code". */
  int Code_name_index;

  /** Constant pool index of "LocalVariableTable". */
  int LocalVariableTable_name_index;

  /** Constant pool index of "LineNumberTable". */
  int LineNumberTable_name_index;

  /** Constant pool index of "SourceFile". */
  int SourceFile_name_index;

  /**
   * Add a new field to this class.
   */
  public Field addField () { return new Field (this); }

  /**
   * Add a new field to this class, and name the field.
   * @param name the name of the mew field
   */
  public Field addField (String name) {
    Field field = new Field (this);
    field.setName(name);
    return field;
  }

  public final Field addField (String name, Type type) {
    Field field = new Field (this);
    field.setName(name);
    field.type = type;
    return field;
  }
  public final Field addField (String name, Type type, int flags)
  {
    Field field = addField (name, type);
    field.setName(name);
    field.type = type;
    field.flags = flags;
    return field;
  }

  Method methods;
  int methods_count;
  Method last_method;
  public Method constructor;

  Method addMethod () {
    return new Method (this, 0);
  }

  public Method addMethod (String name) {
    Method method = new Method (this, 0);
    method.setName(name);
    return method;
  }

  public Method addMethod (String name, int flags) {
    Method method = new Method (this, flags);
    method.setName(name);
    return method;
  }

  public Method addMethod (String name,
			   Type[] arg_types, Type return_type,
			   int flags) {
    Method method = new Method (this, flags);
    method.setName(name);
    method.arg_types = arg_types;
    method.return_type = return_type;
    return method;
  }

  public void doFixups () {
    if (super_class < 0)
      setSuper ("java.lang.Object");
    for (Field field = fields; field != null; field = field.next) {
      field.assign_constants (this);
    }
    for (Method method = methods; method != null; method = method.next) {
      method.assign_constants ();
      method.finalize_labels ();
    }
  }

  public void writeToStream (OutputStream stream)
    throws java.io.IOException
  {
    DataOutputStream dstr = new DataOutputStream (stream);
    int i;

    doFixups ();

    dstr.writeInt (0xcafebabe);  // magic
    dstr.writeShort (minor_version);
    dstr.writeShort (major_version);

    // Write out the constant pool.
    dstr.writeShort (constant_pool_count+1);
    for (i = 1; i <= constant_pool_count; i++) {
	CpoolEntry entry = constant_pool[i];
	if (entry != null)
	    entry.write (dstr);
    }

    dstr.writeShort (access_flags);
    dstr.writeShort (this_class);
    dstr.writeShort (super_class);
    if (interfacesImplemented == null)
      dstr.writeShort (0);  // interfaces_count
    else
      {
	int interfaces_count = interfacesImplemented.length;
	dstr.writeShort (interfaces_count);
	for (i = 0;  i < interfaces_count; i++)
	  dstr.writeShort (interfacesImplemented[i]);
      }

    dstr.writeShort (fields_count);
    for (Field field = fields;  field != null;  field = field.next)
      field.write (dstr, this);

    dstr.writeShort (methods_count);
    for (Method method = methods;  method != null;  method = method.next)
      method.write (dstr, this);

    int attributes_count = sourcefile_index > 0 ? 1 : 0;
    dstr.writeShort (attributes_count);
    if (sourcefile_index > 0)
      {
	dstr.writeShort (SourceFile_name_index);
	dstr.writeInt (2);
	dstr.writeShort (sourcefile_index);
      }
  }

  public void writeToFile (String filename)
    throws java.io.IOException
 {
    FileOutputStream stream = new FileOutputStream (filename);
    writeToStream (stream);
    stream.close ();
  }

  public void writeToFile ()
    throws java.io.IOException
  {
    writeToFile (this_name.replace ('.', '/') + ".class");
  }

  public byte[] writeToArray ()
    throws java.io.IOException
  {
    ByteArrayOutputStream stream = new ByteArrayOutputStream (500);
    writeToStream(stream);
    return stream.toByteArray ();    
  }

  /**
   * Convert a String to a Utf8 byte array.
   * @param str the input String.
   * @return the input encoded as a utf8 byte array.
   */
  public static byte[] to_utf8 (String str)
  {
    if (str == null)
      return null;
    int str_len = str.length ();
    int utf_len = 0;
    for (int i = 0; i < str_len; i++) {
      int c = str.charAt(i);
      if ((c > 0) && (c <= 0x7F))
	utf_len++;
      else if (c <= 0x7FF)
	utf_len += 2;
      else
	utf_len += 3;
    }
    byte[] buffer = new byte[utf_len];
    int j = 0;
    for (int i = 0; i < str_len; i++) {
      int c = str.charAt(i);
      if ((c > 0) && (c <= 0x7F))
	buffer[j++] = (byte) c;
      else if (c <= 0x7FF) {
	buffer[j++] = (byte) (0xC0 | ((c >>  6) & 0x1F));
	buffer[j++] = (byte) (0x80 | ((c >>  0) & 0x3F));
      } else {
	buffer[j++] = (byte) (0xE0 | ((c >> 12) & 0x0F));
	buffer[j++] = (byte) (0x80 | ((c >>  6) & 0x3F));
	buffer[j++] = (byte) (0x80 | ((c >>  0) & 0x3F));
      }
    }
    return buffer;
  }

  /** Compile (in given method) cast from Object to this Type. */
  public void compileCoerceFromObject (Method method)
  {
    if (this == Type.string_type)
      method.compile_invoke_virtual (Type.toString_method);
    else if (this != Type.pointer_type)
      method.compile_checkcast (this);
  }
}
