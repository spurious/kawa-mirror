// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class Type {
  String signature;
  // Fully-qualified name (in external format, i.e. using '.' to separate).
  String this_name;
  /**
   * Nominal unpromoted size in bytes.
   */
  int size;

  Type () { }

  public final String getSignature () { return signature; }
  protected void setSignature(String sig) { this.signature = sig; }

  Type (String nam, String sig) {
    this_name = nam;
    signature = sig;
    size = 4;
  }

  Type (String nam, String sig, int siz) {
    this_name = nam;
    signature = sig;
    size = siz;
  }

  public Type promote () {
    return size < 4 ? int_type : this;
  }

  /** Returns the primitive type corresponding to a signature character.
   * @return a primitive type, or null if there is no such type. */
  public static Type signatureToPrimitive(char sig)
  {
    switch(sig)
      {
      case 'B':  return Type.byte_type;
      case 'C':  return Type.char_type;
      case 'D':  return Type.double_type;
      case 'F':  return Type.float_type;
      case 'S':  return Type.short_type;
      case 'I':  return Type.int_type;
      case 'J':  return Type.long_type;
      case 'Z':  return Type.boolean_type;
      case 'V':  return Type.void_type;
      }
    return null;
  }

  /** Get a Type corresponding to the given signature string. */
  public static Type signatureToType(String sig, int off, int len)
  {
    if (len == 0)
      return null;
    char c = sig.charAt(off);
    Type type;
    if (len == 1)
      {
	type = signatureToPrimitive(c);
	if (type != null)
	  return type;
      }
    if (c == '[')
      {
	type = signatureToType(sig, off+1, len-1);
	return type == null ? null : new ArrayType(type);
      }
    if (c == 'L' && len > 2 && sig.indexOf(';', off) == len-1+off)
      return ClassType.make(sig.substring(off+1,len-1+off).replace('/', '.'));
    return null;
  }

  /** Get a Type corresponding to the given signature string. */
  public static Type signatureToType(String sig)
  {
    return signatureToType(sig, 0, sig.length());
  }

  /** Return the length of the signature starting at a given string position.
   * Returns -1 for an invalid signature. */
  public static int signatureLength (String sig, int pos)
  {
    int len = sig.length();
    if (len <= pos)
      return -1;
    char c = sig.charAt(pos);
    int arrays = 0;
    while (c == '[')
      {
	arrays++;
	pos++;
	c = sig.charAt(pos);
      }
    if (signatureToPrimitive(c) != null)
      return arrays+1;
    if (c == 'L')
      {
	int end = sig.indexOf(';', pos);
	if (end > 0)
	  return arrays + end + 1 - pos;
      }
    return -1;
  }

  public static int signatureLength (String sig)
  {
    return signatureLength(sig, 0);
  }

  /** Returns the Java-level type name from a given signature.
   * Returns null for an invalid signature. */
  public static String signatureToName(String sig)
  {
    int len = sig.length();
    if (len == 0)
      return null;
    char c = sig.charAt(0);
    Type type;
    if (len == 1)
      {
	type = signatureToPrimitive(c);
	if (type != null)
	  return type.getName();
      }
    if (c == '[')
      {
	int arrays = 1;
	if (arrays < len && sig.charAt(arrays) == '[')
	  arrays++;
	sig = signatureToName(sig.substring(arrays));
	if (sig == null)
	  return null;
	StringBuffer buf = new StringBuffer(50);
	buf.append(sig);
	while (--arrays >= 0)
	  buf.append("[]");
	return buf.toString();
      }
    if (c == 'L' && len > 2 && sig.indexOf(';') == len-1)
      return sig.substring(1,len-1).replace('/', '.');
    return null;
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
  = pointer_type.addMethod ("toString", typeArray0,
			     string_type, Access.PUBLIC);
  static public ClassType number_type = new ClassType ("java.lang.Number");
  static public Method intValue_method
  = number_type.addMethod ("intValue", typeArray0,
			    int_type, Access.PUBLIC);
  static public Method longValue_method
  = number_type.addMethod ("longValue", typeArray0,
			    long_type, Access.PUBLIC);
  static public Method floatValue_method
  = number_type.addMethod ("floatValue", typeArray0,
			    float_type, Access.PUBLIC);
  static public Method doubleValue_method
  = number_type.addMethod ("doubleValue", typeArray0,
			    double_type, Access.PUBLIC);
  static public Method booleanValue_method
  = boolean_ctype.addMethod ("booleanValue", typeArray0,
			      boolean_type, Access.PUBLIC);
}
