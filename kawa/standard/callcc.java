package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "call-with-current-continuation".
 * This is a restricted version, that only works for escape-like applications.
 * @author Per Bothner
 */

public class callcc extends Procedure1
{
  /** Call a precedure with the current continuation. */
  public static Object apply (Procedure proc)
  {
    kawa.lang.Continuation cont = new kawa.lang.Continuation ();
    try
      {
	return proc.apply1 (cont);
      }
    catch (CalledContinuation ex)
      {
	if (ex.continuation != cont)
	  throw ex;
	return ex.value;
      }
    finally
      {
	cont.invoked = true;
      }
  }

  public Object apply1 (Object arg1)
  {
    Procedure proc;
    try
      {
	proc = (Procedure) arg1;
      }
    catch (ClassCastException ex)
      {
	throw new GenericError ("argument to call/cc is not procedure");
      }
    return apply (proc);
  }

}
