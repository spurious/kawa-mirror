// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class Field extends Location implements AttrContainer {
  int flags;
  Field next;

  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
    { this.attributes = attributes; }

  /** The class that contains this field. */
  ClassType owner;

  /** If non-null, the interned source-file (unmangled) name of the field. */
  String sourceName;

  /** If non-null, a cached version of the Field for reflectivion. */
  java.lang.reflect.Field rfield;

  /** Add a new Field to a ClassType. */
  public Field (ClassType ctype)
  {
    if (ctype.last_field == null)
      ctype.fields = this;
    else
      ctype.last_field.next = this;
    ctype.last_field = this;
    ctype.fields_count++;
    owner = ctype;
  }

  public final ClassType getDeclaringClass()
  {
    return owner;
  }

  public final void setStaticFlag (boolean is_static) {
    if (is_static)
      flags |= Access.STATIC;
    else
      flags ^= ~Access.STATIC;
  }

  public final boolean getStaticFlag () {
    return (flags & Access.STATIC) != 0;
  }

  public final int getFlags() {
    return flags;
  }
  
  public final int getModifiers() {
    return flags;
  }
  
  void write (DataOutputStream dstr, ClassType classfile)
       throws java.io.IOException
  {
    dstr.writeShort (flags);
    dstr.writeShort (name_index);
    dstr.writeShort (signature_index);

    Attribute.writeAll(this, dstr);
  }
  
  void assign_constants (ClassType classfile)
  {
    ConstantPool constants = classfile.constants;
    if (name_index == 0 && name != null)
      name_index = constants.addUtf8(name).index;
    if (signature_index == 0 && type != null)
      signature_index = constants.addUtf8(type.signature).index;
    Attribute.assignConstants(this, classfile);
  }

  public java.lang.reflect.Field getReflectField()
    throws java.lang.NoSuchFieldException
  {
    if (rfield == null)
      rfield = owner.getReflectClass().getDeclaredField(getName());
    return rfield;
  }

  public void setSourceName(String name)
  {
    sourceName = name;
  }

  public String getSourceName()
  {
    if (sourceName == null)
      sourceName = getName().intern();
    return sourceName;
  }

  /** Find a field with the given name.
   * @param fields list of fields to search
   * @param name (interned source) name of field to look for
   */
  public static Field searchField(Field fields, String name)
  {
    for (; fields != null;  fields = fields.next)
      {
	if (fields.getSourceName() == name)
	  return fields;
      }
    return null;
  }

  public final Field getNext()
  {
    return next;
  }

}
