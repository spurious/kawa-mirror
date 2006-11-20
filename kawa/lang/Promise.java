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

  public void print (Consumer out)
  {
    if (result == null)
      out.append("#<promise - not forced yet>");
    else
      {
	out.append("#<promise - forced to a ");
	out.append(result.getClass().getName());
	out.append ('>');
      }
  }
}
