package kawa.lang;
import gnu.mapping.*;

/**
 * A Continuation "represents an entire (default) future for the computation.
 * This implemementation is based on Java exceptions, and is restricted
 * to "upward" (?) continuation (i.e. catch/throw-type uses).
 * @author	Per Bothner
 */

public class Continuation extends MethodProc
{
  public boolean invoked;
  static int counter;

  public Continuation (CallContext ctx)
  {
  }

  public void apply (CallContext ctx)
  {
    if (invoked)
      throw new GenericError
	("implementation restriction: continuation can only be used once");
    throw new CalledContinuation (ctx.values, this);
  }

  public final String toString()
  {
    return "#<continuation " + id + (invoked ? " (invoked)>" : ">");
  }
}

