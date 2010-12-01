// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.*;
import java.io.*;
/* #ifdef use:javax.lang.model */
import javax.lang.model.element.*;
/* #endif */

/** Represents a "RuntimeVisibleAnnotations" or "RuntimeInvisibleAnnotations" attribute. */

public class RuntimeAnnotationsAttr extends Attribute
  /* #ifdef use:javax.lang.model */
  /* FUTURE: implements AnnotationValueVisitor<Object,CodeAttr> */
  /* #endif */
{
  int dataLength;

  int numEntries;
  AnnotationEntry[] entries;

  /** Add a new AnnotationAttr to a Member. */
  public RuntimeAnnotationsAttr(String name, AnnotationEntry[] entries,
                                int numEntries, AttrContainer container)
  {
    super(name);
    this.entries = entries;
    this.numEntries = numEntries;
    addToFrontOf(container);
  }

  /** Return the length of the attribute in bytes.
    * Does not include the 6-byte header (for the name_index and the length).*/
  public int getLength() { return dataLength; }

  public void print (ClassTypeWriter dst)
  {
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.print(getLength());
    dst.print(", number of entries: ");
    dst.println(numEntries);
    for (int i = 0;  i < numEntries;  i++)
      {
        entries[i].print(2, dst);
      }
  }

  static AnnotationEntry readAnnotationEntry (java.io.DataInputStream dstr, ConstantPool constants)
    throws java.io.IOException
  {
    AnnotationEntry aentry = new AnnotationEntry();
    int tindex = dstr.readUnsignedShort();
    CpoolEntry cpentry = constants.getForced(tindex, ConstantPool.UTF8);
    aentry.annotationTypeIndex = tindex;
    int count = dstr.readUnsignedShort();
    for (int i = 0;  i < count;  i++)
      {
        int nindex = dstr.readUnsignedShort();
        cpentry = constants.getForced(nindex, ConstantPool.UTF8);
        AnnotationEntry.Value value = readAnnotationValue(dstr, constants);
        value.nindex = nindex;
        aentry.addMember(((CpoolUtf8) cpentry).getString(), value);
      }
    return aentry;
  }

  static AnnotationEntry.Value readAnnotationValue (java.io.DataInputStream dstr, ConstantPool constants)
       throws java.io.IOException
  {
    byte kind = dstr.readByte();
    int expected = 0;
    AnnotationEntry.Value val = new AnnotationEntry.Value((char) kind, null);
    CpoolEntry cpentry;
    int index;
    Object value;
    switch (kind)
      {
      case 'B':
      case 'S':
      case 'I':
      case 'Z':
      case 'C':
        val.index1 = dstr.readUnsignedShort();
        cpentry = constants.getForced(val.index1, ConstantPool.INTEGER);
        int ivalue = ((CpoolValue1) cpentry).value;
        if (kind == 'I') val.value = Integer.valueOf(ivalue);
        else if (kind == 'S') val.value = Short.valueOf((short) ivalue);
        else if (kind == 'B') val.value = Byte.valueOf((byte) ivalue);
        else if (kind == 'Z') val.value = Boolean.valueOf(ivalue != 0);
        else /*kind=='C'*/  val.value = Character.valueOf((char) ivalue);
        return val;
      case 'J':
        val.index1 = dstr.readUnsignedShort();
        cpentry = constants.getForced(val.index1, ConstantPool.LONG);
        val.value = Long.valueOf(((CpoolValue2) cpentry).value);
        return val;
      case 'F':
        val.index1 = dstr.readUnsignedShort();
        cpentry = constants.getForced(val.index1, ConstantPool.FLOAT);
        val.value = Float.valueOf(Float.intBitsToFloat(((CpoolValue1) cpentry).value));
        return val;
      case 'D':
        val.index1 = dstr.readUnsignedShort();
        cpentry = constants.getForced(val.index1, ConstantPool.DOUBLE);
        val.value = Double.valueOf(Double.longBitsToDouble(((CpoolValue2) cpentry).value));
        return val;
      case 's': // String
        val.index1 = dstr.readUnsignedShort();
        cpentry = constants.getForced(val.index1, ConstantPool.UTF8);
        val.value = ((CpoolUtf8) cpentry).getString();
        return val;
      case 'e': // enum constant
        val.index1 = dstr.readUnsignedShort();
        val.index2 = dstr.readUnsignedShort();
        cpentry = constants.getForced(val.index1, ConstantPool.UTF8);
        String cname = ((CpoolUtf8) cpentry).getString();
        cpentry = constants.getForced(val.index2, ConstantPool.UTF8);
        String ename = ((CpoolUtf8) cpentry).getString();
        val.value = new String[] { cname, ename };
        return val;
      case 'c':
        val.index1 = dstr.readUnsignedShort();
        cpentry = constants.getForced(val.index1, ConstantPool.UTF8);
        val.value = ((CpoolUtf8) cpentry).getString();
        return val;
      case '[': // array
        int count = dstr.readUnsignedShort();
        List<AnnotationEntry.Value> values
          = new ArrayList<AnnotationEntry.Value>(count);
        for (int i = 0; i < count;  i++)
          values.add(readAnnotationValue(dstr, constants));
        val.value = values;
        return val;
      case '@':
        val.value = readAnnotationEntry(dstr, constants);
        return val;
      default:
        ;
      }
    return null;
  }

  public void assignConstants (ClassType cl)
  {
    super.assignConstants(cl);
    for (int i = 0;  i < numEntries;  i++)
      dataLength += assignConstants(entries[i], cl.getConstants());
  }

  static int assignConstants (AnnotationEntry aentry, ConstantPool constants)
  {
    Map<String,AnnotationEntry.Value> map = aentry.elementsValue;
    int dlen = 4;
    aentry.annotationTypeIndex = constants.addUtf8(aentry.annotationType.getSignature()).index;
    for (Map.Entry<String,AnnotationEntry.Value> e : map.entrySet())
      {
        AnnotationEntry.Value val = e.getValue();
        val.nindex = constants.addUtf8(e.getKey()).index;
        dlen += 2;
        dlen += assignConstants(val, constants);
      }
    return dlen;
  }

  static int assignConstants (AnnotationEntry.Value val, ConstantPool constants)
  {
    Object value = val.getValue();
    switch (val.kind)
      {
      case 'B':
      case 'S':
      case 'I':
        if (val.index1 == 0)
          val.index1 = constants.addInt(((Number) value).intValue()).index;
        return 3;
      case 'J':
        if (val.index1 == 0)
          val.index1 = constants.addLong(((Long) value).longValue()).index;
        return 3;
      case 'F':
        if (val.index1 == 0)
          val.index1 = constants.addFloat(((Float) value).floatValue()).index;
        return 3;
      case 'D':
        if (val.index1 == 0)
          val.index1 = constants.addDouble(((Double) value).doubleValue()).index;
        return 3;
      case 'Z':
        if (val.index1 == 0)
          val.index1 = constants.addInt(((Boolean) value).booleanValue() ? 1 : 0).index;
        return 3;
      case 'C':
        if (val.index1 == 0)
          val.index1 = constants.addInt((int)((Character) value).charValue()).index;
        return 3;
      case 's':
        if (val.index1 == 0)
          val.index1 = constants.addString((String) value).index;
        return 3;
      case '[':
        int dlen = 3;
        List<AnnotationEntry.Value> vals = (List<AnnotationEntry.Value>) value;
        int sz = vals.size();
        for (int i = 0;  i < sz;  i++)
          dlen += assignConstants(vals.get(i), constants);
        return dlen;
      case 'e':
        String cname, ename;
        if (value instanceof Field)
          {
            Field fld = (Field) value;
            cname = fld.getDeclaringClass().getInternalName();
            ename = fld.getName();
          }
        else
          {
            String[] sarr = (String[]) value;
            cname = sarr[0];
            ename = sarr[0];
          }
        if (val.index1 == 0)
          val.index1 = constants.addUtf8(cname).index;
        if (val.index2 == 0)
          val.index2 = constants.addUtf8(ename).index;
        return 5;
      case 'c':
        if (val.index1 == 0)
          {
            String str = value instanceof String ? (String) value
              : ((ClassType) value).getSignature();
            val.index1 = constants.addUtf8(str).index;
          }
        return 3;
      case '@':
        return 1 + assignConstants((AnnotationEntry) value, constants);
      default:
        throw new UnsupportedOperationException();
      }
  }

  public void write (DataOutputStream dstr) throws java.io.IOException
  {
    for (int i = 0;  i < numEntries;  i++)
      write(entries[i], getConstants(), dstr);
  }

  static void write (AnnotationEntry aentry, ConstantPool constants, DataOutputStream dstr) throws java.io.IOException

  {
    dstr.writeShort(aentry.annotationTypeIndex);
    Map<String,AnnotationEntry.Value> map = aentry.elementsValue;
    dstr.writeShort(map.size());
    for (Map.Entry<String,AnnotationEntry.Value> e : map.entrySet())
      {
        AnnotationEntry.Value val = e.getValue();
        dstr.writeShort(val.nindex);
        write(val, constants, dstr);
      }
  }

  static void write (AnnotationEntry.Value val, ConstantPool constants, DataOutputStream dstr) throws java.io.IOException
  {
    Object value = val.getValue();
    int kind = val.kind;
    dstr.writeByte((byte)kind);
    switch (kind)
      {
      case 'B':
      case 'S':
      case 'I':
      case 'J':
      case 'F':
      case 'D':
      case 'Z':
      case 'C':
      case 's':
        dstr.writeShort(val.index1);
      case '[':
        List<AnnotationEntry.Value> vals = (List<AnnotationEntry.Value>) value;
        int sz = vals.size();
        dstr.writeShort(sz);
        for (int i = 0;  i < sz;  i++)
          write(vals.get(i), constants, dstr);
        break;
      case 'e':
        Field fld = (Field) value;
        dstr.writeShort(val.index1);
        dstr.writeShort(val.index2);
        break;
      case 'c':
        dstr.writeShort(constants.addUtf8(((ClassType) value).getSignature()).index);
        break;
      case '@':
        write((AnnotationEntry) value, constants, dstr);
        break;
      default:
        throw new UnsupportedOperationException();
      }
    }
}
