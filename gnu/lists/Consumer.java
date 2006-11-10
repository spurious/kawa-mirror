// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer is something that will accept data (output),
 * and do something with it.
 * A consumer is like a SAX DocumentHandler or a PrintWriter,
 * but more abstract.  If a Sequence class impleemnts Consumer,
 * then data "written" to the sequence will be inserted in the sequence.
 * <p>
 * <em>Note:</em> This interface is not quite final.  For example it is
 * probable we will add methods for comments, processing instructions, etc.
 */

public interface Consumer
  /* #ifdef JAVA5 */
  // extends Appendable
  /* #endif */
{
  public void writeChar(int v);
  public void writeBoolean(boolean v);

  public void writeFloat(float v);
  public void writeDouble(double v);
  public void writeInt(int v);
  public void writeLong(long v);

  public void beginDocument();
  public void endDocument();

  public void beginGroup(Object type);
  public void endGroup();

  /** Write a attribute for the current group.
   * This is only allowed immediately after a beginGroup. */
  public void beginAttribute(Object attrType);

  /** End of an attribute or end of an actual parameter.
   * The former use matches a beginAttribute; the latter may not,
   * and can be used to separate parameters in a parameter list.
   * This double duty suggsts the method should at least be re-named. */
  public void endAttribute();

  public void writeObject(Object v);

  /** True if consumer is ignoring rest of group.
   * The producer can use this information to skip ahead. */
  public boolean ignoring();

  public void writeChars(String str);
  // public void writeChars(AbstractString str);
  public void write(char[] buf, int off, int len);

  /* #ifdef JAVA5 */
  // public Consumer append (char c);
  // public Consumer append (CharSequence csq);
  // public Consumer append (CharSequence csq, int start, int end);
  /* #endif */
}
