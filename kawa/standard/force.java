package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/** Implement the Scheme standard function "force".
 * We provide the first of the extensions listed in R4RS:  Calling force
 *   on an object that is nor a promise may simply return the object.
 * @author Per Bothner
 */

public class force extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Promise)
      return ((Promise)arg1).force ();
    if (arg1 instanceof Future)
      return ((Future)arg1).waitForResult ();
    return arg1;
  }
}
