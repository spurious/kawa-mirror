package kawa.standard;

import kawa.lang.Procedure2;

public class string_equal_p extends kawa.lang.Procedure2 {
   public string_equal_p() {
      super("string=?");
   }

   public Object apply2 (Object arg1, Object arg2)
   {
      if (((java.lang.StringBuffer)arg1).toString().equals(
             ((java.lang.StringBuffer)arg2).toString()
          )) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
