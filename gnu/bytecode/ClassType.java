// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class ClassType extends Type implements AttrContainer {
  public static final int minor_version = 3;
  public static final int major_version = 45;

  static java.util.Hashtable classTable;

  /** Find a ClassType with the given name, or create a new one.
   * Use this for "library classes", where you need the field/method types,
   * but not one where you are about to generate code for.
   * @param name the name of the class (e..g. "java.lang.String").
   */
  static ClassType make(String name)
  {
    if (classTable == null)
      classTable = new java.util.Hashtable();
    ClassType cl = (ClassType) classTable.get(name);
    if (cl == null)
      {
	cl = new ClassType(name);
	classTable.put(name, cl);
      }
    return cl;
  }

  int thisClassIndex;

  /** The super (base) class of the current class.
   * X.superClass == X means the superClass has not been specified,
   * and defaults to java.lang.Object.
   * X.superClass == null means X has no super class, which implies
   * that X == java.lang.Object. */
  ClassType superClass = this;
  /** The constant pool index of the superClass, or -1 if unassigned. */
  int superClassIndex = -1;

  ClassType[] interfaces;
  int[] interfaceIndexes;
  public int access_flags;

  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
    { this.attributes = attributes; }

  String sourcefile;

  boolean emitDebugInfo = true;

  ConstantPool constants;

  public final ConstantPool getConstants () { return constants; }

  public final CpoolEntry getConstant(int i)
  {
    if (constants == null || constants.pool == null
	|| i > constants.count)
      return null;
    return constants.pool[i];
  }

  /** Return the modifiers (access flags) for this class. */
  public final int getModifiers() { return access_flags; }

  /** Set the modifiers (access flags) for this class. */
  public final void setModifiers(int flags) { access_flags = flags; }

  /** Sets the name of the class being defined in this classfile.
   * @param name the name to give to the class
   */
  void setName (String name)
  {
    this_name = name;
    name = name.replace ('.', '/');
    setSignature("L"+name+";");
  }

  /** Set the name of the SourceFile associated with this class. */
  public void setSourceFile (String name)
  {
    SourceFileAttr.setSourceFile(this, name);
  }

  /**
   * Set the superclass of the is class.
   * @param name name of super class, or null if this is "Object".
   */
  public void setSuper (String name)
  {
    setSuper(name == null ? null : ClassType.make(name));
  }

  public void setSuper (ClassType superClass)
  {
    this.superClass = superClass;
  }

  public ClassType[] getInterfaces() { return interfaces; }

  public void setInterfaces (ClassType[] interfaces)
  { this.interfaces = interfaces; }

  public ClassType () { }

  public ClassType (String class_name)
  {
    super();
    size = 4;
    setName(class_name);
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

  public Method addMethod (String name,  String signature, int flags)
  {
    int len = signature.length();
    if (len < 3 || signature.charAt(0) != '(')
      throw new ClassFormatError("bad method signature");
    int pos = 1;
    java.util.Stack types = new java.util.Stack();
    for (;;)
      {
	int arg_sig_len = Type.signatureLength(signature, pos);
	if (arg_sig_len < 0)
	  {
	    if (pos < len && signature.charAt(pos) == ')')
	      break;
	    throw new ClassFormatError("bad method signature");
	  }
	String arg_sig = signature.substring(pos, pos+arg_sig_len);
	Type arg_type = Type.signatureToType(arg_sig);
	types.push(arg_type);
	pos += arg_sig_len;
      }
    Type[] arg_types = new Type[types.size()];
    for (int i = types.size();  --i >= 0; )
      arg_types[i] = (Type) types.pop();
    Type rtype = Type.signatureToType(signature.substring(pos+1));
    return addMethod(name, arg_types, rtype, flags);
  }

  /** Do various fixups after generating code but before we can write it out.
   * This includes assigning constant pool indexes where needed,
   * finalizing labels, etc. */
  public void doFixups ()
  {
    if (constants == null)
      constants = new ConstantPool();
    if (thisClassIndex == 0)
      thisClassIndex = constants.addClass(this).index;
    if (superClass == this)
      setSuper((ClassType) null);
    if (superClassIndex < 0)
      superClassIndex = superClass == null ? 0
	: constants.addClass(superClass).index;
    if (interfaces != null && interfaceIndexes == null)
      {
	int n = interfaces.length;
	interfaceIndexes = new int [n];
	for (int i = 0;  i < n;  i++)
	  interfaceIndexes[i] = constants.addClass(interfaces[i]).index;
      }
    for (Field field = fields; field != null; field = field.next) {
      field.assign_constants (this);
    }
    for (Method method = methods; method != null; method = method.next) {
      method.assign_constants ();
      method.finalize_labels ();
    }
    Attribute.assignConstants(this, this);
  }

  public void writeToStream (OutputStream stream)
    throws java.io.IOException
  {
    java.io.DataOutputStream dstr = new java.io.DataOutputStream (stream);
    int i;

    doFixups ();

    dstr.writeInt (0xcafebabe);  // magic
    dstr.writeShort (minor_version);
    dstr.writeShort (major_version);

    // Write out the constant pool.
    if (constants == null)
      dstr.writeShort (1);
    else
      constants.write(dstr);

    dstr.writeShort (access_flags);
    dstr.writeShort (thisClassIndex);
    dstr.writeShort (superClassIndex);
    if (interfaceIndexes == null)
      dstr.writeShort (0);  // interfaces_count
    else
      {
	int interfaces_count = interfaceIndexes.length;
	dstr.writeShort (interfaces_count);
	for (i = 0;  i < interfaces_count; i++)
	  dstr.writeShort (interfaceIndexes[i]);
      }

    dstr.writeShort (fields_count);
    for (Field field = fields;  field != null;  field = field.next)
      field.write (dstr, this);

    dstr.writeShort (methods_count);
    for (Method method = methods;  method != null;  method = method.next)
      method.write (dstr, this);

    Attribute.writeAll (this, dstr);
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
