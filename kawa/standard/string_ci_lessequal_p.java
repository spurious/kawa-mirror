package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme procedure "string-ci<=?". */

public class string_ci_lessequal_p extends Procedure2
{
   public Object apply2 (Object arg1, Object arg2)
   {
      if (arg1.toString().toLowerCase().compareTo(
             arg2.toString().toLowerCase()
          )<=0) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
