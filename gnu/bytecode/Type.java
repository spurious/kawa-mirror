package codegen;
import java.io.*;

public class Type {
  byte[] name;
  byte[] signature;
  // Fully-qualified name (in external format, i.e. using '.' to separate).
  String this_name;
  /**
   * Nominal unpromoted size in bytes.
   */
  int size;

  Type () { }

  public final byte[] getSignature () { return signature; }

  /**
   * Given a class type, return the corresponing signature string.
   */
  static byte[] class_signature (byte[] class_name) {
    int name_length = class_name.length;
    byte[] sig = new byte[name_length+2];
    sig[0] = 'L';
    System.arraycopy (class_name, 0, sig, 1, name_length);
    sig[name_length + 1] = ';';
    return sig;
  }

  public Type (byte[] nam, byte[] sig)
  {
    this(nam, sig, 4);
  }

  protected Type (byte[] class_name) {
    this (class_name, class_signature (class_name));
  }

  Type (String nam, String sig) {
    this_name = nam;
    name = ClassType.to_utf8 (nam);
    signature = ClassType.to_utf8 (sig);
    size = 4;
  }
  Type (byte[] nam, byte[] sig, int siz)
  {
    name = nam;  signature = sig; size = siz;
    this_name = new String(nam, 0);
  }

  Type (String nam, String sig, int siz) {
    this_name = nam;
    name = ClassType.to_utf8 (nam);
    signature = ClassType.to_utf8 (sig);
    size = siz;
  }

  public Type promote () {
    return size < 4 ? int_type : this;
  }

  public final String getName ()
  {
    return this_name;
  }

  /** Compile code to coerce/convert from Objevt to this type. */
  public void compileCoerceFromObject (Method method)
  {
    if (this == boolean_type)
      {
	method.compile_checkcast (boolean_ctype);
	method.compile_invoke_virtual (booleanValue_method);
	return;
      }
    method.compile_checkcast (number_type);
    if (this == int_type || this == short_type || this == byte_type)
      method.compile_invoke_virtual (intValue_method);
    else if (this == long_type)
      method.compile_invoke_virtual (longValue_method);
    else if (this == double_type)
      method.compile_invoke_virtual (doubleValue_method);
    else if (this == float_type)
      method.compile_invoke_virtual (floatValue_method);
    // Have left out Character -> char, since not used by Kawa.
    else
      throw new Error ("unimplemented compileCoerceFromObject");
  }

  static public Type byte_type = new Type ("byte", "B", 1);
  static public Type short_type = new Type ("short", "S", 2);
  static public Type int_type = new Type ("int", "I", 4);
  static public Type long_type = new Type ("long", "J", 8);

  static public Type float_type = new Type ("float", "F", 4);
  static public Type double_type = new Type ("double", "D", 8);

  static public Type boolean_type = new Type ("boolean", "Z", 1);
  static public Type char_type = new Type ("char", "C", 2);

  static public Type void_type = new Type ("void", "V", 0);

  static public ClassType pointer_type = new ClassType ("java.lang.Object");
  static public ClassType string_type = new ClassType ("java.lang.String");
  static public ClassType boolean_ctype = new ClassType ("java.lang.Boolean");
  static public Type[] typeArray0 = new Type[0];
  static public Method toString_method
  = pointer_type.new_method ("toString", typeArray0,
			     string_type, Access.PUBLIC);
  static public ClassType number_type = new ClassType ("java.lang.Number");
  static public Method intValue_method
  = number_type.new_method ("intValue", typeArray0,
			    int_type, Access.PUBLIC);
  static public Method longValue_method
  = number_type.new_method ("longValue", typeArray0,
			    long_type, Access.PUBLIC);
  static public Method floatValue_method
  = number_type.new_method ("floatValue", typeArray0,
			    float_type, Access.PUBLIC);
  static public Method doubleValue_method
  = number_type.new_method ("doubleValue", typeArray0,
			    double_type, Access.PUBLIC);
  static public Method booleanValue_method
  = boolean_ctype.new_method ("booleanValue", typeArray0,
			      boolean_type, Access.PUBLIC);
}
