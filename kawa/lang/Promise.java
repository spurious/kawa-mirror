package kawa.lang;

/** Implement Scheme "promises".
 * @author Per Bothner
 */

public class Promise implements Printable
{
  Procedure0 thunk;

  /** The result - or null if it is not ready. */
  Object result;

  /** Create a new Promise that will evaluata think when forced. */
  public Promise (Procedure0 thunk)
  {
    this.thunk = thunk;
  }

  public Object force ()
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
    ps.print ("#<promise ");
    if (result == null)
      ps.print (" - not forced yet>");
    else
      {
	SFormat.print (result, ps);
	ps.print ('>');
      }
  }
}
