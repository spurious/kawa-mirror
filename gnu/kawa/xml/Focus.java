// Copyright (C) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.math.IntNum;

public final class Focus extends TreePosition
{
  /* BEGIN JAVA2 */
  static ThreadLocal current = new ThreadLocal();
  /* END JAVA2 */
  /* BEGIN JAVA1 */
  // static Focus current = new Focus();
  /* END JAVA1 */

  public static Focus getCurrent()
  {
    /* BEGIN JAVA2 */
    Object obj = current.get();
    if (obj == null)
      {
	obj = new Focus();
	current.set(obj);
      }
    return (Focus) obj;
    /* END JAVA2 */
    /* BEGIN JAVA1 */
    // return current;
    /* END JAVA1 */
  }

  public long position;
  IntNum contextPosition;
  int size;
  IntNum contextSize;

  public static void compileGetCurrent(Compilation comp)
  {
    // FIXME This should be optimized so it only done once per method.
    CodeAttr code = comp.getCode();
    code.emitInvoke(getCurrentFocusMethod);
  }

  public static final ClassType TYPE = ClassType.make("gnu.kawa.xml.Focus");
  static final Method getCurrentFocusMethod
    = TYPE.getDeclaredMethod("getCurrent", 0);
}
