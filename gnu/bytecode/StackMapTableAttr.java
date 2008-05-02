// Copyright (c) 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** Represents a "StackMapTable" attribute, as added in Java 6. */

public class StackMapTableAttr extends Attribute
{
  byte[] data;
  int numEntries;

  /** Add a new StackMapTableAttr to a CodeAttr. */
  public StackMapTableAttr(int numEntries, byte[] data, CodeAttr code)
  {
    super("StackMapTable");
    addToFrontOf(code);
    this.numEntries = numEntries;
    this.data = data;
  }

  /** Return the length of the attribute in bytes.
    * Does not include the 6-byte header (for the name_index and the length).*/
  public int getLength() { return 2 + data.length; }

  /** Write out the contents of the Attribute.
    * Does not write the 6-byte attribute header. */
  public void write (java.io.DataOutputStream dstr)
    throws java.io.IOException
  {
    dstr.writeShort(numEntries);
    dstr.write(data, 0, data.length);
  }

  private int u1(int offset)
  {
    return data[offset] & 0xFF;
  }

  private int u2(int offset)
  {
    return ((data[offset]  & 0xFF) << 8) + (data[offset+1] & 0xFF);
  }

  /** Extract a single verification_type_info.
   * @param startOffset starting index in data
   * @return offset in data after consuming one verification type
   */
  int print_verification_type (int startOffset, ClassTypeWriter dst)
  {
    int consumed = 1;
    switch (data[startOffset])
      {
      case 0: // ITEM_Top
        dst.print("top/unavailable");
        break;
      case 1:  // ITEM_Integer
        dst.print("integer");
        break;
      case 2:  // ITEM_Float
        dst.print("float");
        break;
      case 3:  // ITEM_Double
        dst.print("double");
        break;
      case 4:  // ITEM_Long
        dst.print("long");
        break;
      case 5:  // ITEM_Null
        dst.print("null");
        break;
      case 6:  // ITEM_UniniializedThis
        dst.print("uninitialized this");
        break;
      case 7:  // ITEM_Object
        int index = u2(startOffset+1);
        dst.printOptionalIndex(index);
        dst.printConstantTersely(index, ConstantPool.CLASS);
        consumed += 2;
        break;
      case 8:  // ITEM_uninitialized
        int offset = u2(startOffset+1);
        consumed += 2;
        dst.print("uninitialized object created at ");
        dst.print(offset);
        break;
      default:
        throw new Error("bad verification type tag");
      }
    return startOffset + consumed;

  }

  int print_verification_types (int startOffset, int count, ClassTypeWriter dst)
  {
    int offset = startOffset;
    while (--count >= 0)
      {
        dst.print("    ");
        offset = print_verification_type(offset, dst);
        dst.println();
      }
    return offset;
  }

  public void print (ClassTypeWriter dst)
  {
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.print(getLength());
    dst.print(", number of entries: ");
    dst.println(numEntries);
    int ipos = 0;
    int pc_offset = -1;
    for (int i = 0;  i < numEntries;  i++)
      {
        int tag = u1(ipos++);
        pc_offset++;
        if (tag <= 127)
          pc_offset += tag & 63;
        else
          {
            pc_offset += u2(ipos);
            ipos += 2;
          }
        dst.print("  offset: ");
        dst.print(pc_offset);
        if (tag <= 63)
          dst.println(" - same_frame");
        else if (tag <= 127)
          {
            dst.println(" - same_locals_1_stack_item_frame");
            ipos = print_verification_types(ipos, 1, dst);
          }
        else if (tag <= 246)
          {
            dst.println(" - tag reserved for future use");
            break;
          }
        else if (tag == 247)
          {
            dst.println(" - same_locals_1_stack_item_frame_extended");
            ipos = print_verification_types(ipos, 1, dst);
          }
        else if (tag <= 250)
          dst.println(" - chop_frame");
        else if (tag == 251)
          dst.println(" - same_frame_extended");
        else if (tag <= 254)
          {
            dst.println(" - append_frame");
            int count = tag - 251;
            ipos = print_verification_types(ipos, count, dst);
          }
        else // tag == 255
          {
            int num_locals = u2(ipos);
            ipos += 2;
            dst.print(" - full_frame.  Locals: ");
            dst.println(num_locals);
            ipos = print_verification_types(ipos, num_locals, dst);
            int num_stack = u2(ipos);
            ipos += 2;
            dst.print("    (end of locals)");
            // Align "Locals:" and "Stack length:":
            for (int nspaces = Integer.toString(pc_offset).length();
                 --nspaces >= 0; )
              dst.print(' ');
            dst.print("       Stack length: ");
            dst.println(num_stack);
            ipos = print_verification_types(ipos, num_stack, dst);
          }
      }
  }
}
