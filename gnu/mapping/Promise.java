package gnu.mapping;
import gnu.text.Printable;
import gnu.lists.Consumer;

/** Implement Scheme "promises".
 * @author Per Bothner
 */

public class Promise<T> implements Printable, Lazy<T>
{
  Procedure thunk;

  /** The result - or UNBOUND if it is not ready. */
  private volatile Object result = Location.UNBOUND;
  private Throwable throwable;

  /** Create a new Promise that will evaluate thunk when forced. */
  public Promise (Procedure thunk)
  {
    this.thunk = thunk;
  }

  /** Wrap value as a forced Promise. */
  public static <T> Lazy<T> makeBoundPromise (T value) {
      Promise<T> p = new Promise<T>(null);
      p.result = value;
      return p;
  }

  public T getValue ()
  {
    // Doesn't reliably work on JDK4 or earlier, but works on JDK5 or later.
    // http://www.cs.umd.edu/~pugh/java/memoryModel/DoubleCheckedLocking.html
    if (result == Location.UNBOUND)
      {
        synchronized (this)
          {
            if (result == Location.UNBOUND && throwable == null)
              {
                try
                  {
                    Object x = thunk.apply0 ();
                    if (result == Location.UNBOUND)
                      result = x;
                  }
                catch (Throwable ex)
                  {
                    throwable = ex;
                  }
              }
            if (throwable != null)
		WrappedException.rethrow(throwable);
          }
      }
    return (T) result;
  }

    /** Forces the argment, if needed, to a non-Lazy valyue.
     * I.e. calls {@link Lazy#getValue} as many times as needed.
     */
    public static Object force (Object arg) {
	while (arg instanceof Lazy)
	    arg = ((Lazy) arg).getValue();
	return arg;
    }

  public void print (Consumer out)
  {
    Object r = result;
    if (r == Location.UNBOUND)
      {
        synchronized (this)
          {
            if (throwable != null)
              {
                out.write("#<promise - force threw a ");
                out.write(throwable.getClass().getName());
                out.write('>');
              }
            else
              out.write("#<promise - not forced yet>");
          }
      }
    else if (r == null)
      out.write("#<promise - forced to null>");
    else
      {
	out.write("#<promise - forced to a ");
	out.write(r.getClass().getName());
	out.write ('>');
      }
  }
}
