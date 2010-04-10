package kawa.lang;
import gnu.mapping.*;
import gnu.text.Printable;
import gnu.lists.Consumer;

/** Implement Scheme "promises".
 * @author Per Bothner
 */

public class Promise implements Printable
{
  Procedure thunk;

  /** The result - or null if it is not ready. */
  Object result;

  /** Create a new Promise that will evaluate thunk when forced. */
  public Promise (Procedure thunk)
  {
    this.thunk = thunk;
  }

  public Object force () throws Throwable
  {
    if (result == null)
      {
	Object x = thunk.apply0 ();
	if (result == null)
	  result = x;
      }
    return result;
  }

  public static Object force (Object arg) throws Throwable
  {
    if (arg instanceof Promise)
      return ((Promise) arg).force();
    if (arg instanceof gnu.mapping.Future)
      return ((gnu.mapping.Future) arg).waitForResult();
    /* #ifdef JAVA5 */
    if (arg instanceof java.util.concurrent.Future<?>)
      return ((java.util.concurrent.Future<?>) arg).get();
    /* #endif */
    return arg;
  }

  public void print (Consumer out)
  {
    if (result == null)
      out.write("#<promise - not forced yet>");
    else
      {
	out.write("#<promise - forced to a ");
	out.write(result.getClass().getName());
	out.write ('>');
      }
  }
}
