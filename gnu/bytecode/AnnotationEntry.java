// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.*;

/** A placeholder for future annotation support. */

public class AnnotationEntry
/* #ifdef JAVA5 */
implements java.lang.annotation.Annotation
/* #endif */
/* #ifdef JAVA6 */
// /* FUTURE also implements: javax.lang.model.element.AnnotationMirror */
/* #endif */
{
  ClassType annotationType;
  LinkedHashMap<String,Object> elementsValue = new LinkedHashMap<String,Object>(10);

  public ClassType getAnnotationType ()
  {
    return annotationType;
  }

  public void addMember(String name, Object value)
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
    for (Map.Entry<String,Object> it : elementsValue.entrySet())
      {
        String key = it.getKey();
        Object value1 = it.getValue();
        Object value2 = other.elementsValue.get(key);
        if (value1 != value2)
          {
            if (value1 == null || value2 == null
                || ! value1.equals(value2))
              return false;
          }
      }
    for (Map.Entry<String,Object> it : other.elementsValue.entrySet())
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
    for (Map.Entry<String,Object> it : elementsValue.entrySet())
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
    for (Map.Entry<String,Object> it : elementsValue.entrySet())
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
}
