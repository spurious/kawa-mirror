package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "call-with-current-continuation".
 * This is a restricted version, that only works for escape-like applications.
 * @author Per Bothner
 */

public class callcc extends Procedure1
{
  public callcc()
  {
    super("call-with-current-continuation");
  }

  public Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
