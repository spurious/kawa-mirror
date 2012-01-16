package gnu.mapping;
import gnu.text.Printable;
import gnu.lists.Consumer;

/** Implement Scheme "promises".
 *
 * This is a final class, because in some cases (as required by SRFI-45)
 * we copy the fields of one Promise from other Promise, and such
 * copying would be dubious if either operand is a sub-class.
 *
 * @author Per Bothner
 */

public final class Promise<T> implements Printable, Lazy<T>
{
    Procedure thunk;

    /** If getValue yields another Lazy value, call getValue on that too. */
    boolean forceValueIfPromise;

    /** Set whether to recursive call getValue if getValue yields a Laxy value. */
    public void setForceValueIfPromise(boolean value) {
        forceValueIfPromise = value;
    }

    /** The result - or UNBOUND if it is not ready. */
    private volatile Object result = Location.UNBOUND;

    /** If an exception was thrown when getting value, store it here. */
    private Throwable throwable;

    /** Create new "blank" Promise.
     * Calling getValue just waits until one of setValue, setAlias,
     * setException or setThunk is invoked.
     * One of these is typically done by some other "producer" thread.
     */
    public Promise() {
    }

    /** Create a new Promise that will evaluate thunk when forced. */
    public Promise(Procedure thunk) {
        this.thunk = thunk;
    }

    /** Wrap value as a forced Promise. */
    public static <T> Lazy<T> makeBoundPromise (T value) {
        Promise<T> p = new Promise<T>(null);
        p.result = value;
        return p;
    }

    public static  Lazy<Object> coerceToLazy (Object value) {
        if (value instanceof Lazy<?>)
            return (Lazy<Object>) value;
        Promise<Object> p = new Promise<Object>(null);
        p.result = value;
        return p;
    }

    public T getValue () {
        Object r = result;
        // Doesn't reliably work on JDK4 or earlier, but works on JDK5 or later.
        // http://www.cs.umd.edu/~pugh/java/memoryModel/DoubleCheckedLocking.html
        if (r != Location.UNBOUND)
            return (T) r;
        for (;;) {
            // Invariant: r == this.result
            synchronized (this) {
                while (r == Location.UNBOUND && throwable == null) {
                    if (thunk == null) {
                        try {
                            wait();
                        } catch (java.lang.InterruptedException ex) {
                        }
                        r = result;
                    } else {
                        try {
                            r = thunk.apply0 ();
                            if (result == Location.UNBOUND)
                                result = r;
                            else
                                r = result;
                        } catch (Throwable ex) {
                            throwable = ex;
                        }
                        thunk = null;
                        break;
                    }
                }
                if (throwable != null)
                    WrappedException.rethrow(throwable);
            }
            if (! forceValueIfPromise || ! (r instanceof Lazy))
                return (T) r;
            if (! (r instanceof Promise)) {
                return ((Lazy<T>) r).getValue();
            }
            synchronized(r) {
                moveFrom((Promise) r);
            }
            r = result;
        }
    }

    /** Copy fields of other into this, and set other to indirect to this.
     * This is used to implement the SRFI-45 requirements.
     */
    private void moveFrom(Promise other) {
        this.thunk = other.thunk;
        this.forceValueIfPromise = other.forceValueIfPromise;
        this.throwable = other.throwable;
        this.result = other.result;

        other.result = this;
        other.forceValueIfPromise = true;
        other.thunk = null;
        other.throwable = null;
    }

    public void checkUnset() {
         if (result != Location.UNBOUND || throwable != null || thunk != null)
             throw new IllegalStateException();
    }

    /** Bind this promise to a given (eager) value. */
    public synchronized void setValue(Object value) {
         checkUnset();
         result = value;
         notifyAll();
    }

    /** Bind promise to be an alias of another Lazy value. */
    public synchronized void setAlias(Lazy promise) {
         checkUnset();
         result = promise;
         setForceValueIfPromise(true);
         notifyAll();
    }

    /** Bind this promise so forcing it throws the given exception. */
    public synchronized void setException(Throwable exception) {
         checkUnset();
         throwable = exception;
         notifyAll();
    }

    /** Bind this promise so forcing it evaluates the given procedure. */
    public synchronized void setThunk(Procedure thunk) {
         checkUnset();
         this.thunk = thunk;
         notifyAll();
    }

    /** Forces the argument, if needed.
     * I.e. calls {@link Lazy#getValue} once if the argument is Lazy.
     */
    public static Object force1 (Object arg) {
	if (arg instanceof Lazy)
	    arg = ((Lazy) arg).getValue();
	return arg;
    }

    /** Forces the argument, if needed, to a non-Lazy value.
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
