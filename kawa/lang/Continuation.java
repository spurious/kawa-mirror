package kawa.lang;

/**
 * A Continuation "rerpesents an entire (default) future for the computation.
 * This implemementation is based on Java exceptions, and is restricted
 * to "upward" (?) continuation (i.e. catch/throw-type uses).
 * @author	Per Bothner
 */

public class Continuation extends Procedure1
{
  boolean invoked;
  static int counter;
  int id;

  public Continuation ()
  {
    id = ++counter;
  }

  public Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (invoked)
      throw new GenericError
	("implementation restriction: continuation can be invoked once");
    throw new CalledContinuation (arg1, this);
  }

  /** Call a precedure with the current continuation. */
  public static Object callcc (Procedure proc)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Continuation cont = new Continuation ();
    try
      {
	return proc.apply1 (cont);
      }
    catch (CalledContinuation ex)
      {
	if (ex.continuation == cont)
	  return ex.value;
	throw ex;
      }
    finally
      {
	cont.invoked = true;
      }
  }

  public final String toString()
  {
    return "#<continuation " + id + (invoked ? " (invoked)>" : ">");
  }
}

