package kawa.standard;
import kawa.lang.*;

public class string_greaterequal_p extends Procedure2 {
   public string_greaterequal_p() {
      super("string>=?");
   }

   public Object apply2 (Object arg1, Object arg2)
   {
      if (((java.lang.StringBuffer)arg1).toString().compareTo(
             ((java.lang.StringBuffer)arg2).toString()
          )>=0) {
         return Interpreter.trueObject;
      } else {
         return Interpreter.falseObject;
      }
   }

}
