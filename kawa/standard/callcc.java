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
    return Continuation.callcc (proc);
  }

}
