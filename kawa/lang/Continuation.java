package kawa.lang;
import gnu.mapping.*;

/**
 * A Continuation "rerpesents an entire (default) future for the computation.
 * This implemementation is based on Java exceptions, and is restricted
 * to "upward" (?) continuation (i.e. catch/throw-type uses).
 * @author	Per Bothner
 */

public class Continuation extends ProcedureN
{
  public boolean invoked;
  static int counter;
  int id;

  public Continuation ()
  {
    id = ++counter;
  }

  public Object apply1 (Object arg1)
  {
    if (invoked)
      throw new GenericError
	("implementation restriction: continuation can only be used once");
    throw new CalledContinuation (arg1, this);
  }

  public Object applyN (Object[] args)
  {
    return apply1 (Values.make (args));
  }

  public final String toString()
  {
    return "#<continuation " + id + (invoked ? " (invoked)>" : ">");
  }
}

