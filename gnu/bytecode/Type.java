package codegen;
import java.io.*;

public class Type {
  byte[] name;
  byte[] signature;
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
    name = nam;  signature = sig; size = 4;
  }

  protected Type (byte[] class_name) {
    this (class_name, class_signature (class_name));
  }

  Type (String nam, String sig) {
    name = ClassType.to_utf8 (nam);
    signature = ClassType.to_utf8 (sig);
    size = 4;
  }
  Type (byte[] nam, byte[] sig, int siz)
  { name = nam;  signature = sig; size = siz; }
  Type (String nam, String sig, int siz) {
    name = ClassType.to_utf8 (nam);
    signature = ClassType.to_utf8 (sig);
    size = siz;
  }

  public Type promote () {
    return size < 4 ? int_type : this;
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
}
