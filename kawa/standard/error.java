package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard DSSSL procedure "error". */

public class error extends Procedure1
{
   public Object apply1 (Object arg1) throws GenericError
   {
     throw new GenericError(arg1.toString());
   }
}
