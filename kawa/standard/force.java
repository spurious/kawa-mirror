package kawa.standard;
import kawa.lang.*;

/** Implement the Scheme standard function "force".
 * We provide the first of the extensions listed in R4RS:  Calling force
 *   on an object that is nor a promise may simply return the object.
 * @author Per Bothner
 */

public class force extends Procedure1
{
  public Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (arg1 instanceof Promise)
      return ((Promise)arg1).force ();
    return arg1;
  }
}
