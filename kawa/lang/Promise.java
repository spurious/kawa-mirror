package kawa.lang;
import gnu.mapping.*;

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

  public void print(java.io.PrintWriter ps)
  {
    if (result == null)
      ps.print("#<promise - not forced yet>");
    else
      {
	ps.print("#<promise - forced to a ");
	ps.print(result.getClass().getName());
	ps.print ('>');
      }
  }
}
