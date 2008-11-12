// Copyright (c) 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** Represents a "StackMapTable" attribute, as added in Java 6. */

public class StackMapTableAttr extends Attribute
{
  byte[] data;
  int dataLength;
  int numEntries;

  /** Add a new StackMapTableAttr to a CodeAttr. */
  public StackMapTableAttr(int numEntries, byte[] data, CodeAttr code)
  {
    super("StackMapTable");
    addToFrontOf(code);
    this.numEntries = numEntries;
    this.data = data;
    this.dataLength = data.length;
  }

  public Method getMethod() { return ((CodeAttr) container).getMethod(); }

  /** Return the length of the attribute in bytes.
    * Does not include the 6-byte header (for the name_index and the length).*/
  public int getLength() { return 2 + dataLength; }

  /** Write out the contents of the Attribute.
    * Does not write the 6-byte attribute header. */
  public void write (java.io.DataOutputStream dstr)
    throws java.io.IOException
  {
    dstr.writeShort(numEntries);
    dstr.write(data, 0, dataLength);
  }

  private int u1(int offset)
  {
    return data[offset] & 0xFF;
  }

  private int u2(int offset)
  {
    return ((data[offset]  & 0xFF) << 8) + (data[offset+1] & 0xFF);
  }

  void printVerificationType (int encoding, ClassTypeWriter dst)
  {
    int tag = encoding & 0xff;
    switch (tag)
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
        int index = encoding >> 8;
        dst.printOptionalIndex(index);
        dst.printConstantTersely(index, ConstantPool.CLASS);
        break;
      case 8:  // ITEM_uninitialized
        int offset = encoding >> 8;
        dst.print("uninitialized object created at ");
        dst.print(offset);
        break;
      default:
        dst.print("<bad verification type tag "+tag+'>');
      }
  }

  /** Extract a single verification_type_info.
   * @param startOffset starting index in data
   * @return encoded verification type
   */
  int extractVerificationType (int startOffset, int tag)
  {
    if (tag == 7 || tag == 8)
      {
        int value = u2(startOffset+1);
        tag |= (value << 8);
      }
    return tag;
  }

  static int[] reallocBuffer (int[] buffer, int needed)
  {
    if (buffer == null)
      buffer = new int[needed+10];
    else if (needed > buffer.length)
      {
        int[] tmp = new int[needed+10];
        System.arraycopy(buffer, 0, tmp, 0, buffer.length);
        buffer = tmp;
      }
    return buffer;
  }

  int extractVerificationTypes (int startOffset, int count, int startIndex,
                                int[] buffer)
  {
    int offset = startOffset;
    while (--count >= 0)
      {
        int encoding;
        if (offset >= dataLength)
          encoding = -1;
        else
          {
            int tag = data[offset];
            encoding = extractVerificationType(offset, tag);
            offset += (tag == 7 || tag == 8 ? 3 : 1);
          }
        buffer[startIndex++] = encoding;
      }
    return offset;
  }

  /** Print a sequence of encoded verification types.
   * @param startIndex index if of encodings of first type to print
   * @param count number of entries in encodings to print
   */
  void printVerificationTypes (int[] encodings, int startIndex, int count,
         ClassTypeWriter dst)
  {
    int regno = 0;
    for (int i = 0;  i < startIndex + count;  i++)
      {
        int encoding = encodings[i];
        int tag = encoding & 0xff;
        if (i >= startIndex)
          {
            dst.print("   ");
            if (regno >= 100)
              ;
            else
              {
                if (regno >= 10)
                  dst.print(' ');
                dst.print(' ');
              }
            dst.print(regno);
            dst.print(": ");
            printVerificationType(encoding,  dst);
            dst.println();
          }
        regno++;
        if (tag == 3 || tag == 4)
          regno++;
      }
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
    Method method = getMethod();
    int encodedTypes[] = null;
    int curLocals = (method.getStaticFlag() ? 0 : 1) + method.arg_types.length;
    int curStack = 0;
    for (int i = 0;  i < numEntries;  i++)
      {
        if (ipos >= dataLength)
          {
            i = -1;
            break;
          }
        int tag = u1(ipos++);
        pc_offset++;
        int delta = -1;
        if (tag <= 127)
          pc_offset += tag & 63;
        else if (ipos + 1 >= dataLength)
          {
            ipos = -1;
            break;
          }
        else
          {
            delta = u2(ipos);
            pc_offset += delta;
            ipos += 2;
          }
        dst.print("  offset: ");
        dst.print(pc_offset);
        if (tag <= 63)
          {
            dst.println(" - same_frame");
            curStack = 0;
          }
        else if (tag <= 127 || tag == 247)
          {
            dst.println(tag <= 127 ? " - same_locals_1_stack_item_frame"
                        : " - same_locals_1_stack_item_frame_extended");
            encodedTypes = reallocBuffer(encodedTypes, 1);
            ipos = extractVerificationTypes(ipos, 1, 0, encodedTypes);
            printVerificationTypes(encodedTypes, 0, 1, dst);
            curStack = 1;
          }
        else if (tag <= 246)
          {
            dst.println(" - tag reserved for future use");
            break;
          }
        else if (tag <= 250)
          {
            int count = 251-tag;
            dst.print(" - chop_frame - undefine ");
            dst.print(count);
            dst.println(" locals");
            curLocals -= count;
            curStack = 0;
          }
        else if (tag == 251)
          {
            dst.println(" - same_frame_extended");
            curStack = 0;
          }
        else if (tag <= 254)
          {
            int count = tag - 251;
            dst.print(" - append_frame - define ");
            dst.print(count);
            dst.println(" more locals");
            encodedTypes = reallocBuffer(encodedTypes, curLocals+count);
            ipos = extractVerificationTypes(ipos, count, curLocals, encodedTypes);
            printVerificationTypes(encodedTypes, curLocals, count, dst);
            curLocals += count;
            curStack = 0;
          }
        else // tag == 255
          {
            if (ipos + 1 >= dataLength)
              {
                ipos = -1;
                break;
              }
            int num_locals = u2(ipos);
            ipos += 2;
            dst.print(" - full_frame.  Locals count: ");
            dst.println(num_locals);
            encodedTypes = reallocBuffer(encodedTypes, num_locals);
            ipos = extractVerificationTypes(ipos, num_locals, 0, encodedTypes);
            printVerificationTypes(encodedTypes, 0, num_locals, dst);
            curLocals = num_locals;
            if (ipos + 1 >= dataLength)
              {
                ipos = -1;
                break;
              }
            int num_stack = u2(ipos);
            ipos += 2;
            dst.print("    (end of locals)");
            // Align "Locals count:" and "Stack count:":
            for (int nspaces = Integer.toString(pc_offset).length();
                 --nspaces >= 0; )
              dst.print(' ');
            dst.print("       Stack count: ");
            dst.println(num_stack);
            encodedTypes = reallocBuffer(encodedTypes, num_stack);
            ipos = extractVerificationTypes(ipos, num_stack, 0, encodedTypes);
            printVerificationTypes(encodedTypes, 0, num_stack, dst);
            curStack = num_stack;
          }
        if (ipos < 0)
          {
            dst.println("<ERROR - missing data>");
            return;
          }
      }
  }
}
