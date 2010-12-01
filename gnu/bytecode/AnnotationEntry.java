// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.*;
/* #ifdef use:javax.lang.model */
import javax.lang.model.element.*;
/* #endif */

/** An annotation value mirror. */

public class AnnotationEntry
/* #ifdef JAVA5 */
implements java.lang.annotation.Annotation
/* #endif */
/* #ifdef use:javax.lang.model */
/* FUTURE also implements: javax.lang.model.element.AnnotationMirror */
/* #endif */
{
  ClassType annotationType;
  int annotationTypeIndex;

  LinkedHashMap<String,Value> elementsValue = new LinkedHashMap<String,Value>(10);

  public ClassType getAnnotationType ()
  {
    return annotationType;
  }

  public void addMember(String name, Value value)
  {
    elementsValue.put(name, value);
  }

  /* #ifdef JAVA5 */
  @SuppressWarnings("unchecked")
  /* #endif */
  public Class<? extends java.lang.annotation.Annotation> annotationType ()
  {
    return (Class<? extends java.lang.annotation.Annotation>) annotationType.getReflectClass();
  }

  /* FUTURE
  public Map<Member,AnnotationValue> getElementValues()
  {
      convert from elementsValue;
  }
  */

  public boolean equals(Object obj)
  {
    if (! (obj instanceof AnnotationEntry))
      return false;
    AnnotationEntry other = (AnnotationEntry) obj;
    if (! getAnnotationType().getName().equals(other.getAnnotationType().getName()))
      return false;
    for (Map.Entry<String,Value> it : elementsValue.entrySet())
      {
        String key = it.getKey();
        Value value1 = it.getValue();
        Value value2 = other.elementsValue.get(key);
        if (value1 != value2)
          {
            if (value1 == null || value2 == null
                || ! value1.equals(value2))
              return false;
          }
      }
    for (Map.Entry<String,Value> it : other.elementsValue.entrySet())
      {
        String key = it.getKey();
        Object value2 = it.getValue();
        Object value1 = elementsValue.get(key);
        if (value1 != value2)
          {
            if (value1 == null || value2 == null
                || ! value1.equals(value2))
              return false;
          }
      }
    return true;
  }

  public int hashCode()
  {
    int hash = 0;
    // Note the Annotation spec requires we also include the
    // hashCode of members with default values; I don't think we do that.
    for (Map.Entry<String,Value> it : elementsValue.entrySet())
      {
        int khash = it.getKey().hashCode();
        int vhash = it.getValue().hashCode();
        hash += 127 * khash ^ vhash;
      }
    return hash;
  }

  public String toString()
  {
    StringBuilder sbuf = new StringBuilder();
    sbuf.append('@');
    sbuf.append(getAnnotationType().getName());
    sbuf.append('(');
    int count = 0;
    for (Map.Entry<String,Value> it : elementsValue.entrySet())
      {
        if (count > 0)
          sbuf.append(", ");
        sbuf.append(it.getKey());
        sbuf.append('=');
        sbuf.append(it.getValue());
        count++;
      }
    sbuf.append(')');
    return sbuf.toString();
  }

  public void print (int indentation, ClassTypeWriter dst)
  {
    dst.printSpaces(indentation);
    dst.printOptionalIndex(annotationTypeIndex);
    dst.print('@');
    String cname = annotationType != null ? annotationType.getSignature()
      : ((CpoolUtf8) dst.ctype.constants.getPoolEntry(annotationTypeIndex)).getString();
    Type.printSignature(cname, 0, cname.length(), dst);
    int count = elementsValue.size();
    dst.println();
    indentation += 2;
    for (Map.Entry<String,Value> e : elementsValue.entrySet())
      {
        String key = e.getKey();
        Value val = e.getValue();
        dst.printSpaces(indentation);
        dst.printOptionalIndex(val.nindex);
        dst.print(key);
        dst.print(" => ");
        val.print(indentation, dst);
        dst.println();
      }
  }

  public static class Value
  /* #ifdef use:javax.lang.model */
  implements AnnotationValue
  /* #endif */
  {
    /** Either one of the standard primitize signature code
     * B (byte), S (short), I (int) , J (long), F (float), D (double),
     * Z (boolean), C (char).
     * or:
     * 'e' (enum-constant)
     * '[' (array)
     * '@' (type)
     * 's' (string)
     * 'c' (string)
     */
    char kind;

    Object value;

    /** Indexes in ConstantPool of corresponding name. */
    int nindex;
    /** Indexes in ConstantPool, if non-zero. */
    int index1;
    int index2;

    public Value (char kind, Object value)
    {
      this.kind = kind;
      this.value = value;
    }

    /** Get an Object representing the annotation value.
     * If the kind is 'e', the value is *either* a Field or an 2-element
     * array [ClassName, EnumName].
     * If kind is 'c', the value is *either* a ClassType or a String.
     */
    public Object getValue() { return value; }

    public String toString() { return value.toString(); } // FIXME
 
    /* #ifdef use:javax.lang.model */
    public <R,P> R accept(AnnotationValueVisitor<R,P> v, P p)
    {
      switch (kind)
        {
        case 'Z':  return v.visitBoolean(((Boolean) value).booleanValue(), p);
        case 'C':  return v.visitChar(((Character) value).charValue(), p);
        case 'B':  return v.visitByte(((Byte) value).byteValue(), p);
        case 'S':  return v.visitShort(((Short) value).shortValue(), p);
        case 'I':  return v.visitInt(((Integer) value).intValue(), p);
        case 'J':  return v.visitLong(((Long) value).longValue(), p);
        case 'F':  return v.visitFloat(((Float) value).floatValue(), p);
        case 'D':  return v.visitDouble(((Double) value).doubleValue(), p);
        case 's':  return v.visitString((String) value, p);
        case '[':  return v.visitArray((List<? extends AnnotationValue>) value, p);
        case 'e': /* FIXME:   return v.visitEnumConstant((Field) value, p);*/
        case '@':
        default:
          throw new UnsupportedOperationException();
        }
    }
    /* #endif */

    public void print (int indentation, ClassTypeWriter out)
    {
    if ((out.flags & ClassTypeWriter.PRINT_EXTRAS) != 0)
      {
        out.print("(kind:");
        if (kind >= 'A' && kind <= 'z')
          out.print(kind);
        else
          out.print((int) kind);
        out.print(") ");
      }
    int expected = 0;
    switch (kind)
      {
      case 'B':
      case 'I':
      case 'S':
      case 'C':
      case 'Z':
      case 'J':
      case 'D':
      case 'F':
      case 's': // String
        out.printOptionalIndex(out.getCpoolEntry(index1));
        if (value instanceof String)
          out.printQuotedString((String) value);
        else
          out.print(value.toString());
        break;
      case 'e': // enum constant
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
            ename = sarr[1];
          }
        out.print("enum[");
        if ((out.flags & ClassTypeWriter.PRINT_EXTRAS) != 0)
          out.print("type:");
        out.printOptionalIndex(index1);
        Type.printSignature(cname, 0, cname.length(), out);
        if ((out.flags & ClassTypeWriter.PRINT_EXTRAS) != 0)
          out.print(" value:");
        else
          out.print(' ');
        out.printOptionalIndex(index2);
        out.print(ename);
        out.print("]");
        break;
      case 'c': // class
        out.printOptionalIndex(index1);
        cname = value instanceof String ? (String) value
          : ((ClassType) value).getSignature();
        Type.printSignature(cname, 0, cname.length(), out);
        break;
      case '@': // annotation type
        out.println();
        out.printSpaces(indentation + 2);
        ((AnnotationEntry) value).print(indentation + 2, out);
        break;
      case '[': // array
        List<AnnotationEntry.Value> vals = (List<AnnotationEntry.Value>) value;
        int sz = vals.size();
        out.print("array length:");
        out.print(sz);
        for (int i = 0; i < sz;  i++)
          {
            out.println();
            out.printSpaces(indentation + 2);
            out.print(i);
            out.print(": ");
            vals.get(i).print(indentation + 2, out);
          }
        break;
      }
    }
  }
}
