// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** Represents a "RuntimeVisibleAnnotations" or "RuntimeInvisibleAnnotations" attribute. */

public class RuntimeAnnotationsAttr extends MiscAttr
{
  int numEntries;

  /** Add a new AnnotationAttr to a `Member. */
  public RuntimeAnnotationsAttr(String name, byte[] data, AttrContainer container)
  {
    super(name, data, 0, data.length);
    addToFrontOf(container);
    this.numEntries = u2(0);
  }

  public void print (ClassTypeWriter dst)
  {
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.print(getLength());
    dst.print(", number of entries: ");
    dst.println(numEntries);
    int saveOffset = this.offset;
    this.offset = saveOffset + 2;
    for (int i = 0;  i < numEntries;  i++)
      {
        printAnnotation(2, dst);
      }
    this.offset = saveOffset;
  }

  public void printAnnotation (int indentation, ClassTypeWriter dst)
  {
    int type_index = u2();
    dst.printSpaces(indentation);
    dst.printOptionalIndex(type_index);
    dst.print('@');
    dst.printContantUtf8AsClass(type_index);
    int num_element_value_pairs = u2();
    dst.println();
    indentation += 2;
    for (int i = 0; i < num_element_value_pairs;  i++)
      {
        int element_name_index = u2();
        dst.printSpaces(indentation);
        dst.printOptionalIndex(element_name_index);
        dst.printConstantTersely(element_name_index, ConstantPool.UTF8);
        dst.print(" => ");
        printAnnotationElementValue(indentation, dst);
        dst.println();
      }
  }

  public void printAnnotationElementValue (int indentation, ClassTypeWriter dst)
  {
    int tag = u1();
    if ((dst.flags & ClassTypeWriter.PRINT_EXTRAS) != 0)
      {
        dst.print("[kind:");
        if (tag >= 'A' && tag <= 'z')
          dst.print((char) tag);
        else
          dst.print(tag);
        dst.print("] ");
      }
    int expected = 0;
    switch (tag)
      {
      case 'B':
      case 'I':
      case 'S':
      case 'C':
      case 'Z':
        if (expected == 0) expected = ConstantPool.INTEGER;
        // fall through
      case 'J':
        if (expected == 0) expected = ConstantPool.LONG;
        // fall through
      case 'D':
        if (expected == 0) expected = ConstantPool.DOUBLE;
        // fall through
      case 'F':
        if (expected == 0) expected = ConstantPool.FLOAT;
        // fall through
      case 's': // String
        if (expected == 0) expected = ConstantPool.UTF8;
        // fall through - common code
        int const_value_index = u2();
        CpoolEntry entry = dst.getCpoolEntry(const_value_index);
        dst.printOptionalIndex(entry);
        CpoolValue1 cint;
        if (tag == 'Z' && entry != null
            && entry.getTag() == ConstantPool.INTEGER
            && ((cint = (CpoolValue1) entry).value == 0 || cint.value == 1))
          dst.print(cint.value == 0 ? "false" : "true");
        else
          dst.printConstantTersely(const_value_index, expected);
        break;
      case 'e': // enum constant
        int type_name_index = u2();
        int const_name_index = u2();
        dst.print("enum[");
        if ((dst.flags & ClassTypeWriter.PRINT_EXTRAS) != 0)
          dst.print("type:");
        dst.printOptionalIndex(type_name_index);
        dst.printContantUtf8AsClass(type_name_index);
        if ((dst.flags & ClassTypeWriter.PRINT_EXTRAS) != 0)
          dst.print(" value:");
        else
          dst.print(' ');
        dst.printOptionalIndex(const_name_index);
        dst.printConstantTersely(const_name_index, ConstantPool.UTF8);
        dst.print("]");
        break;
      case 'c': // class
        int class_info_index = u2();
        dst.printOptionalIndex(class_info_index);
        dst.printContantUtf8AsClass(class_info_index);
        break;
      case '@': // annotation type
        dst.println();
        dst.printSpaces(indentation + 2);
        printAnnotation(indentation + 2, dst);
        break;
      case '[': // array
        int num_values = u2();
        dst.print("array length:");
        dst.print(num_values);
        for (int i = 0; i < num_values;  i++)
          {
            dst.println();
            dst.printSpaces(indentation + 2);
            dst.print(i);
            dst.print(": ");
            printAnnotationElementValue(indentation + 2, dst);
          }
        break;
      }
  }
}
