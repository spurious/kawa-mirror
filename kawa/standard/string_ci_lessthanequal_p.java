package kawa.standard;

import kawa.lang.Procedure2;

public class string_ci_lessthanequal_p extends kawa.lang.Procedure2 {
   public string_ci_lessthanequal_p() {
      super("string-ci<=?");
   }

   public Object apply2 (Object arg1, Object arg2)
   {
      if (((java.lang.StringBuffer)arg1).toString().toLowerCase().compareTo(
             ((java.lang.StringBuffer)arg2).toString().toLowerCase()
          )<=0) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
